--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Data.Functor.Identity (Identity)
import qualified Data.Functor.Identity as Control.Monad
import Data.List
import Data.Ord
import Data.Time.Locale.Compat
import Hakyll
import qualified Skylighting

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- 画像用Rules
    match "images/*" $ route idRoute >> compile copyFileCompiler

    -- コードハイライトcss用Rules
    create ["css/highlight.css"] $ route idRoute >> compile (makeItem $ compressCss $ Skylighting.styleToCss Skylighting.pygments)

    -- CSS用Rules
    match "css/*" $ route idRoute >> compile compressCssCompiler

    -- JavaScript用Rules
    match "js/*" $ route idRoute >> compile copyFileCompiler

    -- Atom生成用のRules
    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = constField "description" "feed description" <> defaultContext
            renderAtom feedConf feedCtx . take 5 =<< emptyBodyPosts

    -- Template生成
    match "layouts/*" $ compile templateBodyCompiler
    match "includes/*" $ compile templateBodyCompiler

    -- 各記事の生成
    match "posts/*" $ do
        route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            postsCtx <- recentPostField 5 defaultContext
            pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "layouts/post.html" defaultContext
                >>= loadAndApplyTemplate "layouts/default.html" (postsCtx <> siteCtx)
                >>= relativizeUrls

    -- 過去記事リストの生成
    match "archive.html" $ do
        route idRoute
        compile $ do
            posts <- emptyBodyPosts
            postsCtx <-
                mconcat
                    <$> sequence
                        [ recentPostField 5 defaultContext
                        , return $ listField "posts" (postDateCtx <> defaultContext) (return posts)
                        ]

            getResourceBody
                >>= applyAsTemplate postsCtx
                >>= loadAndApplyTemplate "layouts/default.html" (postsCtx <> siteCtx)
                >>= relativizeUrls

    -- トップページの生成
    match "index.html" $ do
        route idRoute
        compile $ do
            postsCtx <- recentPostField 5 $ teaserField "teaser" "content" <> defaultContext

            getResourceBody
                >>= applyAsTemplate postsCtx
                >>= loadAndApplyTemplate "layouts/default.html" (postsCtx <> siteCtx)
                >>= relativizeUrls

    -- その他ページの作成
    match (fromList ["about.html", "links.html"]) $ do
        route idRoute
        compile $ do
            postsCtx <- recentPostField 5 defaultContext

            getResourceBody
                >>= applyAsTemplate postsCtx
                >>= loadAndApplyTemplate "layouts/default.html" (postsCtx <> siteCtx)
                >>= relativizeUrls

--------------------------------------------------------------------------------
siteCtx :: Context String
siteCtx =
    boolField "comments" (const False)
        <> constField "site_name" "コーヒーと線香と万年筆"
        <> constField "site_description" "自分で調べて試してみるのが、一番自分の力になるんだ"
        <> constField "github" "chupaaaaaaan"
        <> constField "qiita" "chupaaaaaaan"
        <> defaultContext

postDateCtx :: Context String
postDateCtx = dateField "date" "%Y-%m-%d (%a)" <> dateField "year" "%Y"

feedConf :: FeedConfiguration
feedConf =
    FeedConfiguration
        { feedTitle = "コーヒーと線香と万年筆: 最近の投稿"
        , feedDescription = "This feed provides fresh recipes for fresh food!"
        , feedAuthorName = "Takayuki Uchida"
        , feedAuthorEmail = ""
        , feedRoot = "https://chupaaaaaaan.github.io"
        }

--------------------------------------------------------------------------------

-- | Hakyll.Web.Template.List.chronological のIdentifier版
chronological' :: (MonadMetadata m, MonadFail m) => [Identifier] -> m [Identifier]
chronological' = sortByM $ getItemUTC defaultTimeLocale
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = map fst . sortBy (comparing snd) <$> mapM (\x -> fmap (x,) (f x)) xs

-- | Hakyll.Web.Template.List.recentFirst のIdentifier版
recentFirst' :: (MonadMetadata m, MonadFail m) => [Identifier] -> m [Identifier]
recentFirst' = fmap reverse . chronological'

-- | 空の（Identifierだけ持っている）Itemを生成する
emptyItem :: Monoid a => Identifier -> Item a
emptyItem ident = Item{itemIdentifier = ident, itemBody = mempty}

-- | Bodyが空である記事の一覧を生成する
emptyBodyPosts :: (MonadMetadata m, MonadFail m, Monoid a) => m [Item a]
emptyBodyPosts = getMatches "posts/*" >>= fmap (fmap emptyItem) . recentFirst'

-- | recent_post用のContext定義
recentPostField :: (MonadMetadata m, MonadFail m, Monoid a) => Int -> Context a -> m (Context b)
recentPostField n ctx = listField "recent_posts" ctx . return . take n <$> emptyBodyPosts
