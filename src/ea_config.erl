-module(ea_config).

-export_type([config/0]).

%% TODO: extend type to what it actually is.
-type config() :: map().