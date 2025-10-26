{
  lib,
  config,
  ...
}: {
  flake.actions-nix = {
    pre-commit.enable = false;

    defaultValues = {
      jobs.runs-on = "ubuntu-latest";
    };

    workflows = let
    in {
      ".github/workflows/update-nixdots.yaml" = {
        name = "Update emacs in nixdots";

        on.push = {};

        jobs.update = {
          permissions.contents = "read";
          steps = [
            {
              name = "Repository dispatch";
              env = {
                GH_TOKEN = "\${{ secrets.PAT_TOKEN }}";
              };
              run = ''
                gh workflow run build-package.yml \
                  --repo thundertheidiot/nixdots \
                  --ref main \
                  -f package=nixosConfigurations.desktop.config.home-manager.users.thunder.programs.emacs.package \
                  -f flake-input=emacs \
                  -f vps-deploy=false
              '';
            }
          ];
        };
      };
    };
  };
}
