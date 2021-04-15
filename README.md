## Usage:

```
% secrets init path/to/secrets/file.secrets
% secrets add # starts $EDITOR
% secrets edit # starts $EDITOR
% secrets find # starts fuzzy search
% secrets randpass [title for the new entry] # creates a new entry with random password
```

## Reproducible build

1. Have GNU Guix or GNU Guix System
2. `. ok`
3. Or, `. ok find mypass`

This puts you into a reproducible development environment, then builds and runs the project. Your shell will be setup with `ocaml`, `dune`, and all other dependencies etc. You can pass extra arguments they will be passed to the built program.

## Packaging a relocatable binary

1. Have GNU Guix or GNU Guix System
2. `. package`
3. Or, `. package -L ~/projects/myguix`, to use a local Guix package definition

This bundles a relocatable binary, and all of its dependencies, inluding glibc, into a single tar file that can be used on any Linux system. You can pass extra arguments which will be appended to the `guix pack` command. One example is `-L ~/projects/myguix`, which will use a local package definition instead of the one available in guix-channels.scm.

## Notes

To update guix-channels.scm:
```
guix describe --format=channels > guix-channels.scm
```