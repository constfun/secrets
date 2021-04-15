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

## Notes

To update guix-channels.scm:
```
guix describe --format=channels > guix-channels.scm
```