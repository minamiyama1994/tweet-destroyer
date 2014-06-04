import Prelude as P hiding ( lookup , id )
import System.IO
import Data.List hiding ( lookup )
import Control.Concurrent
import qualified Data.ByteString.Char8 as DBC
import Control.Monad
import Web.Twitter.Conduit
import Web.Authenticate.OAuth
import Control.Monad.Logger
import qualified Web.Twitter.Types as TT
import qualified Data.Text as T
import Control.Lens

tokens :: String -> String -> OAuth -- TwitterはアクセスするのにOAuthという仕組みを使っているのですが、その時に必要なConsumerKey/ConsumerSecretを指定します
tokens ck cs = twitterOAuth
    { oauthConsumerKey = DBC.pack ck
    , oauthConsumerSecret = DBC.pack cs
    }
credential :: String -> String -> Credential -- TwitterはアクセスするのにOAuthという仕組みを使っているのですが、その時に必要なAccessToken/AccessTokenSecretを指定します
credential ot ots = Credential
    [ (DBC.pack "oauth_token", DBC.pack ot)
    , (DBC.pack "oauth_token_secret", DBC.pack ots)
    ]
twInfo :: String -> String -> String -> String -> TWInfo -- Twitterにアクセスするのに必要な情報を取得します
twInfo ck cs ot ots = setCredential ( tokens ck cs ) ( credential ot ots ) def

main :: IO ( )
main = do
    putStr "ScreenName(ex. minamiyama1994)を入力 > " >> hFlush stdout
    sn <- getLine
    putStr "ConsumerKeyを入力 > " >> hFlush stdout
    ck <- getLine
    putStr "ConsumerSecretを入力 > " >> hFlush stdout
    cs <- getLine
    putStr "AccessTokenを入力 > " >> hFlush stdout
    ot <- getLine
    putStr "AccessTokenSecretを入力 > " >> hFlush stdout
    ots <- getLine
    putStr "何ふぁぼ以上のツイートを削除せずに残しますか？　nを入力すると、n以上のふぁぼられツイートは削除されません > " >> hFlush stdout
    cnt <- getLine >>= return . read
    putStrLn $ show cnt ++ "ふぁぼ以上のツイートを削除せずに残します"
    hFlush stdout
    ids <- getContents >>= return . P.map ( read . read ) . words
    forM_ ( P.map ( P.map snd ) $ groupBy ( \ ( x1 , _ ) ( x2 , _ ) -> x1 `div` 100 == x2 `div` 100 ) $ zip [ 0 .. ] ids ) $ \ is -> do
        threadDelay $ 15 * 1000 * 1000
        ss <- runNoLoggingT . runTW ( twInfo ck cs ot ots ) $ call $ lookup & id ?~ is
        statuss <- runNoLoggingT . runTW ( twInfo ck cs ot ots ) $ mapM ( \ s -> return ( TT.statusId s , TT.statusFavoriteCount s , TT.userScreenName . TT.statusUser $ s ) ) ss
        mapM_ print statuss
        forM_ ( filter ( \ ( sId , sFC , sName ) -> ( sFC < cnt ) || ( sName /= T.pack sn ) ) statuss ) $ \ ( sId , sFC , sName ) -> do
            runNoLoggingT . runTW ( twInfo ck cs ot ots ) $ call $ destroyId sId
            print ( sId , sFC , sName )
            putStrLn "削除しました"
    putStrLn $ "処理が終了しました"
