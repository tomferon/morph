import Morph.Config
import Morph.Migrator
import Morph.Options

main :: IO ()
main = do
  opts   <- getOptions
  config <- readConfigOrDie opts
  withConnection config $ \conn -> do
    migrate conn $ optsMigrationsDirectory opts
