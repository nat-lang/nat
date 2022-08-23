FROM haskell:9.2.2

WORKDIR /app

COPY app app
COPY fragments fragments
COPY src src
COPY interpreter.cabal .
COPY package.yaml .
COPY Setup.hs .
COPY stack.yaml .
COPY stack.yaml.lock .

RUN curl --proto "=https" --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
ENV PATH "$PATH:/root/.ghcup/bin:$PATH"
RUN stack build

CMD ["stack", "exec", "interpreter-exe"]