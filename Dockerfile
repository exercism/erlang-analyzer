FROM hexpm/erlang:25.3.2.3-alpine-3.18.2 AS erlang

WORKDIR /app
ENV REBAR_VERSION=3.22.0

FROM erlang AS downloader

RUN apk --no-cache add curl; \
    mkdir -p /tmp/tools; \
    curl -L https://github.com/erlang/rebar3/releases/download/${REBAR_VERSION}/rebar3 > /tmp/tools/rebar3; \
    chmod +x /tmp/tools/*

FROM erlang AS build

COPY --from=downloader /tmp/tools/rebar3 rebar3
COPY rebar.config rebar.lock ./

RUN ./rebar3 get-deps

COPY src/ src/

RUN ./rebar3 escriptize

FROM erlang

COPY --from=build /app/_build/default/bin/erlang_analyzer /opt/analyzer/bin/erlang_analyzer
COPY              _docker/analyze.sh                      /opt/analyzer/bin/analyze.sh

ENTRYPOINT ["/opt/analyzer/bin/analyze.sh"]
