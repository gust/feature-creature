module App.AppConfig exposing
  ( Environment (..)
  , InitialConfig
  , AppConfig
  , toAppConfig
  , logMsg
  , logMsg'
  )

type Environment = Development
                 | Production

type alias InitialConfig =
  { environment     : String
  , productsApiPath : String
  , user            : String
  }

type alias AppConfig =
  { env             : Environment
  , logger          : Logger
  , productsApiPath : String
  , user            : String
  }

toAppConfig : InitialConfig -> AppConfig
toAppConfig cfg =
  let env = parseEnv cfg.environment
  in { env             = env
     , logger          = mkLogger env
     , productsApiPath = cfg.productsApiPath
     , user            = cfg.user
     }

parseEnv : String -> Environment
parseEnv env = case env of
  "Production" -> Production
  _            -> Development

type Logger = ConsoleLogger
            | NullLogger

mkLogger : Environment -> Logger
mkLogger env = case env of
  Development -> ConsoleLogger
  Production  -> NullLogger

logMsg : Logger -> String -> a -> a
logMsg logger msg a = case logger of
  NullLogger    -> a
  ConsoleLogger -> Debug.log msg a

logMsg' : AppConfig -> String -> a -> a
logMsg' appConfig = logMsg appConfig.logger
