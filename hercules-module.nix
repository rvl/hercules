{ config, pkgs, lib ? pkgs.lib, ... }:

with lib;

let

  cfg = config.services.hercules;

  baseDir = "/var/lib/hercules";

  hydraConf = pkgs.writeScript "hydra.conf" cfg.extraConfig;
  herculesConf = pkgs.writeScript "hercules.conf" cfg.extraHerculesConfig;

  herculesVarConf = "${baseDir}/hercules/config.yaml";  # symlink
  hydraEnv =
    { HYDRA_DBI = cfg.dbi;
      HYDRA_CONFIG = "${baseDir}/hydra.conf";
      HYDRA_DATA = "${baseDir}";
      HERCULES_CONFIG = "${herculesConf}";
    };

  env =
    { NIX_REMOTE = "daemon";
      SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt"; # Remove in 16.03
      PGPASSFILE = "${baseDir}/pgpass";
      NIX_REMOTE_SYSTEMS = concatStringsSep ":" cfg.buildMachinesFiles;
    } // optionalAttrs (cfg.smtpHost != null) {
      EMAIL_SENDER_TRANSPORT = "SMTP";
      EMAIL_SENDER_TRANSPORT_host = cfg.smtpHost;
    } // hydraEnv // cfg.extraEnv;

  serverEnv = env //
    { HYDRA_TRACKER = cfg.tracker;
      COLUMNS = "80";
      PGPASSFILE = "${baseDir}/pgpass-www"; # grrr
      XDG_CACHE_HOME = "${baseDir}/www/.cache";
    } // (optionalAttrs cfg.debugServer { DBIC_TRACE = "1"; });

  localDB = "dbi:Pg:dbname=hydra;user=hydra;";
  localDBUrl = "postgresql://hydra@/hydra";

  haveLocalDB = cfg.dbi == localDB;

in

{
  ###### interface
  options = {

    services.hercules = rec {

      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to run Hercules/Hydra services.
        '';
      };

      dbi = mkOption {
        type = types.str;
        default = localDB;
        example = "dbi:Pg:dbname=hydra;host=postgres.example.org;user=foo;";
        description = ''
          The DBI string for Hydra database connection.
        '';
      };

      dbUrl = mkOption {
        type = types.str;
        default = localDBUrl;
        example = "postgresql://foo@postgres.example.org/hydra";
        description = ''
          The database connection string. Must match dbi.
        '';
      };

      hydraPackage = mkOption {
        type = types.path;
        default = pkgs.hydra;
        description = "The Hydra package.";
      };

      herculesPackage = mkOption {
        type = types.path;
        #default = pkgs.hydra;
        description = "The Hercules package.";
      };

      frontendPackage = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = "The Hercules package.";
      };

      herculesURL = mkOption {
        type = types.str;
        description = ''
          The base URL for the Hercules webserver instance. Used for links in emails.
        '';
      };

      listenHost = mkOption {
        type = types.str;
        default = "*";
        example = "localhost";
        description = ''
          The hostname or address to listen on or <literal>*</literal> to listen
          on all interfaces.
        '';
      };

      port = mkOption {
        type = types.int;
        default = 3000;
        description = ''
          TCP port the web server should listen to.
        '';
      };

      minimumDiskFree = mkOption {
        type = types.int;
        default = 0;
        description = ''
          Threshold of minimum disk space (GiB) to determine if the queue runner should run or not.
        '';
      };

      minimumDiskFreeEvaluator = mkOption {
        type = types.int;
        default = 0;
        description = ''
          Threshold of minimum disk space (GiB) to determine if the evaluator should run or not.
        '';
      };

      notificationSender = mkOption {
        type = types.str;
        description = ''
          Sender email address used for email notifications.
        '';
      };

      smtpHost = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = ["localhost"];
        description = ''
          Hostname of the SMTP server to use to send email.
        '';
      };

      tracker = mkOption {
        type = types.str;
        default = "";
        description = ''
          Piece of HTML that is included on all pages.
        '';
      };

      logo = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = ''
          Path to a file containing the logo of your Hydra instance.
        '';
      };

      debugServer = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to run the server in debug mode.";
      };

      extraConfig = mkOption {
        type = types.lines;
        description = "Extra lines for the Hydra configuration.";
      };

      extraHerculesConfig = mkOption {
        type = types.lines;
        description = "Extra lines for the Hercules configuration.";
      };

      githubPrivateKeyFile = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = "File which contains the GitHub application private key in PEM format.";
      };

      githubOAuthConsumerId = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "GitHub application OAuth credentials";
      };

      githubOAuthConsumerKey = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "GitHub application OAuth credentials";
      };

      extraEnv = mkOption {
        type = types.attrsOf types.str;
        default = {};
        description = "Extra environment variables for Hydra.";
      };

      gcRootsDir = mkOption {
        type = types.path;
        default = "/nix/var/nix/gcroots/hydra";
        description = "Directory that holds Hydra garbage collector roots.";
      };

      buildMachinesFiles = mkOption {
        type = types.listOf types.path;
        default = [ "/etc/nix/machines" ];
        example = [ "/etc/nix/machines" "/var/lib/hydra/provisioner/machines" ];
        description = "List of files containing build machines.";
      };

      useSubstitutes = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to use binary caches for downloading store paths. Note that
          binary substitutions trigger (a potentially large number of) additional
          HTTP requests that slow down the queue monitor thread significantly.
          Also, this Hydra instance will serve those downloaded store paths to
          its users with its own signature attached as if it had built them
          itself, so don't enable this feature unless your active binary caches
          are absolute trustworthy.
        '';
      };
    };

  };


  ###### implementation

  config = mkIf cfg.enable {

    users.extraGroups.hydra = { };

    users.extraUsers.hydra =
      { description = "Hydra";
        group = "hydra";
        createHome = true;
        home = baseDir;
        useDefaultShell = true;
      };

    users.extraUsers.hydra-queue-runner =
      { description = "Hydra queue runner";
        group = "hydra";
        useDefaultShell = true;
        home = "${baseDir}/queue-runner"; # really only to keep SSH happy
      };

    users.extraUsers.hercules-www =
      { description = "Hercules web server";
        group = "hydra";
        useDefaultShell = true;
      };

    nix.trustedUsers = [ "hydra-queue-runner" ];

    services.hercules.extraConfig =
      ''
        notification_sender = ${cfg.notificationSender}
        compress_num_threads = 0
        gc_roots_dir = ${cfg.gcRootsDir}
        use-substitutes = ${if cfg.useSubstitutes then "1" else "0"}
      '';

    services.hercules.extraHerculesConfig =
      ''
        port: ${toString cfg.port}
        hostname: "${cfg.listenHost}"
        accessLogLevel: Disabled
        secretKeyFile: "${baseDir}/hercules/secret.key"
        databaseConnectionString: "${cfg.dbUrl}"
        dataPath: "${baseDir}"
      '' + optionalString (cfg.githubPrivateKeyFile != null) '';
        gitHubAppPrivateKeyFile: "${cfg.githubPrivateKeyFile}"
      '' + optionalString (cfg.githubOAuthConsumerId != null && cfg.githubOAuthConsumerKey != null) ''
        gitHubAuthInfo:
          id: ${cfg.githubOAuthConsumerId}
          secret: ${cfg.githubOAuthConsumerKey}
      '';

    services.hercules.herculesPackage = mkDefault ((import ./release.nix {}).build.x86_64-linux);
    # services.hercules.frontendPackage = mkDefault ((import ./release.nix {}).frontend);

    environment.systemPackages = [ cfg.hydraPackage cfg.herculesPackage ];

    environment.variables = hydraEnv;

    nix.extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true

      # The default (`true') slows Nix down a lot since the build farm
      # has so many GC roots.
      gc-check-reachability = false
    '';

    systemd.services.hercules-init =
      { wantedBy = [ "multi-user.target" ];
        requires = optional haveLocalDB "postgresql.service";
        after = optional haveLocalDB "postgresql.service";
        environment = env;
        preStart = ''
          mkdir -p ${baseDir}
          chown hydra.hydra ${baseDir}
          chmod 0750 ${baseDir}

          mkdir -m 0700 -p ${baseDir}/hercules
          chown hercules-www.hydra ${baseDir}/hercules

          ln -sf ${herculesConf} ${baseDir}/hercules/config.yaml
          ln -sf ${hydraConf} ${baseDir}/hydra.conf

          mkdir -m 0700 -p ${baseDir}/www
          chown hercules-www.hydra ${baseDir}/www

          mkdir -m 0700 -p ${baseDir}/queue-runner
          mkdir -m 0750 -p ${baseDir}/build-logs
          chown hydra-queue-runner.hydra ${baseDir}/queue-runner ${baseDir}/build-logs

          ${optionalString haveLocalDB ''
            if ! [ -e ${baseDir}/.db-created ]; then
              ${pkgs.sudo}/bin/sudo -u ${config.services.postgresql.superUser} ${config.services.postgresql.package}/bin/createuser hydra
              ${pkgs.sudo}/bin/sudo -u ${config.services.postgresql.superUser} ${config.services.postgresql.package}/bin/createdb -O hydra hydra
              touch ${baseDir}/.db-created
            fi
          ''}

          if [ ! -e ${cfg.gcRootsDir} ]; then
            mkdir -p ${cfg.gcRootsDir}
          fi

          chown hydra.hydra ${cfg.gcRootsDir}
          chmod 2775 ${cfg.gcRootsDir}
        '';
        serviceConfig.ExecStart = "${cfg.herculesPackage}/bin/hercules-init -c ${herculesConf}";
        serviceConfig.PermissionsStartOnly = true;
        serviceConfig.User = "hydra";
        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;
      };

    systemd.services.hercules-server =
      { wantedBy = [ "multi-user.target" ];
        requires = [ "hercules-init.service" ];
        after = [ "hercules-init.service" ];
        environment = serverEnv;
        restartTriggers = [ herculesConf ];
        serviceConfig =
          { ExecStart = "@${cfg.herculesPackage}/bin/hercules hercules -c ${herculesVarConf}";
            User = "hercules-www";
            PermissionsStartOnly = true;
            Restart = "always";
          };
      };

    systemd.services.hydra-queue-runner =
      { wantedBy = [ "multi-user.target" ];
        requires = [ "hercules-init.service" ];
        after = [ "hercules-init.service" "network.target" ];
        path = [ cfg.hydraPackage pkgs.nettools pkgs.openssh pkgs.bzip2 config.nix.package ];
        restartTriggers = [ hydraConf ];
        environment = env // {
          PGPASSFILE = "${baseDir}/pgpass-queue-runner"; # grrr
          IN_SYSTEMD = "1"; # to get log severity levels
        };
        serviceConfig =
          { ExecStart = "@${cfg.hydraPackage}/bin/hydra-queue-runner hydra-queue-runner -v";
            ExecStopPost = "${cfg.hydraPackage}/bin/hydra-queue-runner --unlock";
            User = "hydra-queue-runner";
            Restart = "always";

            # Ensure we can get core dumps.
            LimitCORE = "infinity";
            WorkingDirectory = "${baseDir}/queue-runner";
          };
      };

    systemd.services.hydra-evaluator =
      { wantedBy = [ "multi-user.target" ];
        requires = [ "hercules-init.service" ];
        restartTriggers = [ hydraConf ];
        after = [ "hercules-init.service" "network.target" ];
        path = with pkgs; [ nettools cfg.hydraPackage jq ];
        environment = env;
        serviceConfig =
          { ExecStart = "@${cfg.hydraPackage}/bin/hydra-evaluator hydra-evaluator";
            ExecStopPost = "${cfg.hydraPackage}/bin/hydra-evaluator --unlock";
            User = "hydra";
            Restart = "always";
            WorkingDirectory = baseDir;
          };
      };

    systemd.services.hydra-update-gc-roots =
      { requires = [ "hercules-init.service" ];
        after = [ "hercules-init.service" ];
        environment = env;
        serviceConfig =
          { ExecStart = "@${cfg.hydraPackage}/bin/hydra-update-gc-roots hydra-update-gc-roots";
            User = "hydra";
          };
        startAt = "2,14:15";
      };

    # If there is less than a certain amount of free disk space, stop
    # the queue/evaluator to prevent builds from failing or aborting.
    systemd.services.hydra-check-space =
      { script =
          ''
            if [ $(($(stat -f -c '%a' /nix/store) * $(stat -f -c '%S' /nix/store))) -lt $((${toString cfg.minimumDiskFree} * 1024**3)) ]; then
                echo "stopping Hydra queue runner due to lack of free space..."
                systemctl stop hydra-queue-runner
            fi
            if [ $(($(stat -f -c '%a' /nix/store) * $(stat -f -c '%S' /nix/store))) -lt $((${toString cfg.minimumDiskFreeEvaluator} * 1024**3)) ]; then
                echo "stopping Hydra evaluator due to lack of free space..."
                systemctl stop hydra-evaluator
            fi
          '';
        startAt = "*:0/5";
      };

    # Periodically compress build logs. The queue runner compresses
    # logs automatically after a step finishes, but this doesn't work
    # if the queue runner is stopped prematurely.
    systemd.services.hydra-compress-logs =
      { path = [ pkgs.bzip2 ];
        script =
          ''
            find /var/lib/hydra/build-logs -type f -name "*.drv" -mtime +3 -size +0c | xargs -r bzip2 -v -f
          '';
        startAt = "Sun 01:45";
      };

    services.postgresql.enable = mkIf haveLocalDB true;

    services.postgresql.identMap = optionalString haveLocalDB
      ''
        hydra-users hydra hydra
        hydra-users hydra-queue-runner hydra
        hydra-users hercules-www hydra
        hydra-users root hydra
      '';

    services.postgresql.authentication = optionalString haveLocalDB
      ''
        local hydra all ident map=hydra-users
      '';

  };

}
