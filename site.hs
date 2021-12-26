--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

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

    -- Recent Postsおよびteaser生成用Rules
    match "posts/*" $
        version "postedContents" $ do
            route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
            compile $ pandocCompiler >>= saveSnapshot "content" >>= relativizeUrls

    -- Atom生成用のRules
    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = constField "description" "feed description" <> defaultContext
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "postedContents")
            renderAtom feedConf feedCtx (take 5 posts)

    -- Template生成
    match "layouts/*" $ compile templateBodyCompiler
    match "includes/*" $ compile templateBodyCompiler

    -- 各記事の生成
    match "posts/*" $ do
        route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "postedContents")
            let postsCtx = listField "recent_posts" defaultContext (return $ take 5 posts)

            pandocCompiler
                >>= loadAndApplyTemplate "layouts/post.html" defaultContext
                >>= loadAndApplyTemplate "layouts/default.html" (postsCtx <> siteCtx)
                >>= relativizeUrls

    -- 過去記事リストの生成
    match "archive.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "postedContents")
            let postsCtx =
                    listField "recent_posts" defaultContext (return $ take 5 posts)
                        <> listField "posts" (postDateCtx <> defaultContext) (return posts)

            getResourceBody
                >>= applyAsTemplate postsCtx
                >>= loadAndApplyTemplate "layouts/default.html" (postsCtx <> siteCtx)
                >>= relativizeUrls

    -- トップページの生成
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "postedContents")
            let postsCtx = listField "recent_posts" (teaserField "teaser" "content" <> defaultContext) (return $ take 5 posts)

            getResourceBody
                >>= applyAsTemplate postsCtx
                >>= loadAndApplyTemplate "layouts/default.html" (postsCtx <> siteCtx)
                >>= relativizeUrls

    -- その他ページの作成
    match (fromList ["about.html", "links.html"]) $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "postedContents")
            let postsCtx = listField "recent_posts" defaultContext (return $ take 5 posts)

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
