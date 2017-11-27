{ herculesSrc ? { outPath = ./backend; revCount = 1234; rev = "abcdef"; }
, officialRelease ? false
, shell ? false
}:

with import <nixpkgs/lib>;

let

  pkgs = import <nixpkgs> {};

  genAttrs' = genAttrs [ "x86_64-linux" /* "i686-linux" */ ];

  herculesServer = herculesPkg:
    { config, pkgs, ... }:
    { imports = [ ./hercules-module.nix ];

      virtualisation.memorySize = 1024;
      virtualisation.writableStore = true;

      services.hercules.enable = true;
      services.hercules.herculesPackage = herculesPkg;
      services.hercules.herculesURL = "http://hercules.example.org";
      services.hercules.notificationSender = "admin@hercules.example.org";

      services.postgresql.enable = true;
      services.postgresql.package = pkgs.postgresql95;

      environment.systemPackages = [ pkgs.perlPackages.LWP pkgs.perlPackages.JSON ];
    };

  version = builtins.readFile ./version + "." + toString herculesSrc.revCount + "." + herculesSrc.rev;

in

rec {

  build = genAttrs' (system:
    import ./backend/default.nix {
      pkgs = import <nixpkgs> { inherit system; };
      src = herculesSrc.outPath;
    });

  frontend = genAttrs' (system:
    import ./frontend/default.nix {
       pkgs = import <nixpkgs> { inherit system; };
       backend = build.${system};
     });

  docs = import ./docs/default.nix {
    inherit pkgs;
    backend = build.x86_64-linux;
  };

  tests.install = genAttrs' (system:
    with import <nixpkgs/nixos/lib/testing.nix> { inherit system; };
    simpleTest {
      machine = herculesServer build.${system};
      testScript =
        ''
          $machine->waitForJob("hercules-init");
          $machine->waitForJob("hercules");
          $machine->waitForJob("hydra-evaluator");
          $machine->waitForJob("hydra-queue-runner");
          $machine->waitForOpenPort("3000");
          $machine->succeed("curl --fail http://localhost:3000/");
        '';
    });

  /*
  tests.api = genAttrs' (system:
    with import <nixpkgs/nixos/lib/testing.nix> { inherit system; };
    simpleTest {
      machine = hydraServer build.${system};
      testScript =
        let dbi = "dbi:Pg:dbname=hydra;user=root;"; in
        ''
          $machine->waitForJob("hydra-init");

          # Create an admin account and some other state.
          $machine->succeed
              ( "su - hydra -c \"hydra-create-user root --email-address 'alice\@example.org' --password foobar --role admin\""
              , "mkdir /run/jobset /tmp/nix"
              , "chmod 755 /run/jobset /tmp/nix"
              , "cp ${./tests/api-test.nix} /run/jobset/default.nix"
              , "chmod 644 /run/jobset/default.nix"
              , "chown -R hydra /run/jobset /tmp/nix"
              );

          $machine->succeed("systemctl stop hydra-evaluator hydra-queue-runner");
          $machine->waitForJob("hydra-server");
          $machine->waitForOpenPort("3000");

          # Run the API tests.
          $machine->mustSucceed("su - hydra -c 'perl ${./tests/api-test.pl}' >&2");
        '';
  });
  */
}
