# dotfiles
Quick setup:
```
curl https://raw.githubusercontent.com/rasheedja/dotfiles/master/SETUP.sh | bash
```

## Pi

Pi's configuration lives in its own private repo,
[`rasheedja/pi-config`](https://github.com/rasheedja/pi-config), cloned
alongside this one:

```
personal/git/
  dotfiles/     <- this repo
  pi-config/    <- pi's settings, permission policy, extensions, sandbox
```

It moved out because the sandbox's mount list and the permission policy
describe this machine -- which credentials are bound in, where the repos live,
how the policy is wired -- rather than any preference. That is an attack
surface description, and none of it needed to be public.

What stays here is the wiring: `SETUP.sh` points `~/.pi/agent` at
`pi-config`, and `zshrc`'s `sbx` alias runs the sandbox from there.
