# loom-playground

This isn't a library, yet.

Before we can use Project Loom, we need to understand how. Thought I
might as well share my experiments.

## Running With Loom

You'll need some way to point Leiningen to a compatible java executable.

One option is creating a wrapper script:

```sh
#!/usr/bin/env sh
JAVA_CMD="$HOME/.local/openjdk-16-loom/bin/java" lein "$@"
```

If you use GNU Emacs, you can then set dir-locals to use it:

```elisp
((clojure-mode . ((cider-lein-command . "lein16-loom"))))
```

## Status

Experimental

## Linting

Help clj-kondo help you:

```clojure
{:lint-as {loom-playground.core/with-scope clojure.core/let
           loom-playground.core/in-scope clojure.core/let}}
```

## License

Copyright © 2020 Ben Sless

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
