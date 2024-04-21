{-# LANGUAGE DeriveGeneric #-}
module Command
    where

import Options.Applicative as O
data Command 
    = Accounts
    | Version
    | Reassign { from :: String }
    deriving (Eq, Show)

commandOptions :: Parser Command
commandOptions = subparser
    ( O.command "version" (info (pure Version) (progDesc "show version number"))
    <> O.command "accounts" (info (pure Accounts) (progDesc "show accounts information"))
    <> O.command "reassign" 
        (info (Reassign <$> 
                (strOption (long "from" 
                            <> short 'f'
                            <> metavar "ORIGIN"
                            <> value ""
                            <> help "account name"))) (progDesc "reassign from an account"))
    )

parseCommand :: [String] -> ParserResult Command
parseCommand args = execParserPure (prefs disambiguate) (info commandOptions idm) args

parseCommandIO :: [String] -> IO ()
parseCommandIO args = do
        let p = prefs disambiguate
        print $ p
        print $ execParserPure p (info commandOptions idm) args

