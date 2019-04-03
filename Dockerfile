FROM erlang:21-alpine AS build

WORKDIR /root

ADD . /root

RUN rebar3 escriptize

FROM erlang:21-alpine

COPY --from=build /root/_build/default/bin/erlang_analyzer /opt/analyzer/bin/erlang_analyzer
COPY              _docker/analyze.sh                       /opt/analyzer/bin/analyze.sh

ENTRYPOINT ["/opt/analyzer/bin/analyze.sh"]
