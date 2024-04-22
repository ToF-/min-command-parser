{-# LANGUAGE DeriveGeneric #-}
module Command
    where

import Options.Applicative as O

data Currency = EUR | USD
    deriving (Eq, Show)

data Month = Month Int Int
    deriving (Eq, Show)

data Command 
    = Accounts
    | Version
    | Reassign { from :: String }
    | Transfer { origin :: String, dest :: String }
    | Cash { currency :: Currency }
    | Summary { month :: Month } 
    | Detail {  detailCurrency :: Maybe Currency, account :: Maybe String }  
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
                            <> help "account name")))
                (progDesc "reassign from an account"))
    <> O.command "transfer"
        (info (Transfer <$>
                (strOption (long "from"
                            <> short 'f'
                            <> metavar "ORIGIN"
                            <> help "origin account"))
                <*>
                (strOption (long "to"
                            <> short 't'
                            <> metavar "DESTINATION"
                            <> help "destination account")))
                (progDesc "transfer from an account to another"))
    <> O.command "cash"
        (info (Cash <$>
            (flag' EUR (long "EUR" <> short 'e')
                <|> flag' USD (long "USD" <> short 'u')))
              (progDesc "enter some cash in EUR or USD"))
    <> O.command "summary"
        (info ((\y m -> Summary (Month y m)) 
                 <$> (option auto (long "year" <> short 'y' <> (metavar "YEAR")))
                 <*> (option auto (long "month" <> short 'm' <> (metavar "MONTH"))))
            (progDesc "enter a month period: year month"))
    <> O.command "detail"
        (info ((\c a -> Detail c (if a == "" then Nothing else Just a))
               <$>
                   (flag' (Just EUR) (long "EUR" <> short 'e')
                    <|> flag' (Just USD) (long "USD" <> short 'u')
                    <|> flag' Nothing (long "all-currencies"))
                <*> (strOption (long "account" <> short 'a' <> metavar "ACCOUNT" <> value "" <> help "account name")))
                (progDesc "give detail on an account for a currency"))
               


                
    ) <**> simpleVersioner "v1.2.3"


parseCommand :: [String] -> ParserResult Command
parseCommand args = execParserPure (prefs disambiguate) (info commandOptions idm) args

parseCommandIO :: [String] -> IO ()
parseCommandIO args = do
        let p = prefs disambiguate
        print $ p
        print $ execParserPure p (info commandOptions idm) args

