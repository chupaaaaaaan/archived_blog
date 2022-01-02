--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate)
import Hakyll
import qualified Skylighting
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
            renderAtom feedConf feedCtx . take 5 =<< postList

    -- Template生成
    match "layouts/*" $ compile templateBodyCompiler
    match "includes/*" $ compile templateBodyCompiler

    tagList <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- tag
    tagsRules tagList $ \tag pat -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pat
            postsCtx <- recentPostsField 5 defaultContext
            let ctx =
                    constField "title" title
                        <> listField "posts" (tagCtx tagList <> postDateCtx) (return posts)
                tagCloudCtx = tagListField "tagcloud" tagList

            makeItem ""
                >>= loadAndApplyTemplate "layouts/tags.html" ctx
                >>= loadAndApplyTemplate "layouts/default.html" (tagCloudCtx <> postsCtx <> siteCtx)
                >>= relativizeUrls

    -- 各記事の作成(参照用)
    match "posts/*" $
        version "postList" $ do
            route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
            compile $
                pandocCompiler
                    >>= saveSnapshot "content"
                    >>= relativizeUrls

    -- 各記事の生成
    match "posts/*" $ do
        route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            postsCtx <- recentPostsField 5 defaultContext
            let tagCtx = tagsField "tags" tagList
                tagCloudCtx = tagListField "tagcloud" tagList

            pandocCompiler
                >>= loadAndApplyTemplate "layouts/post.html" (tagCtx <> defaultContext)
                >>= loadAndApplyTemplate "layouts/default.html" (tagCloudCtx <> tagCtx <> postsCtx <> siteCtx)
                >>= relativizeUrls

    -- 過去記事リストの生成
    match "archive.html" $ do
        route idRoute
        compile $ do
            postsCtx <-
                mconcat
                    <$> sequence
                        [ recentPostsField 5 defaultContext
                        , allPostsField (postDateCtx <> defaultContext)
                        ]
            let tagCtx = tagsField "tags" tagList
                tagCloudCtx = tagListField "tagcloud" tagList

            getResourceBody
                >>= applyAsTemplate postsCtx
                >>= loadAndApplyTemplate "layouts/default.html" (tagCloudCtx <> tagCtx <> postsCtx <> siteCtx)
                >>= relativizeUrls

    -- トップページの生成
    match "index.html" $ do
        route idRoute
        compile $ do
            postsCtx <- recentPostsField 5 $ teaserField "teaser" "content" <> defaultContext
            let tagCtx = tagsField "tags" tagList
                tagCloudCtx = tagListField "tagcloud" tagList

            getResourceBody
                >>= applyAsTemplate postsCtx
                >>= loadAndApplyTemplate "layouts/default.html" (tagCloudCtx <> tagCtx <> postsCtx <> siteCtx)
                >>= relativizeUrls

    -- その他ページの作成
    match (fromList ["about.html", "links.html"]) $ do
        route idRoute
        compile $ do
            postsCtx <- recentPostsField 5 defaultContext
            let tagCtx = tagsField "tags" tagList
                tagCloudCtx = tagListField "tagcloud" tagList

            getResourceBody
                >>= applyAsTemplate postsCtx
                >>= loadAndApplyTemplate "layouts/default.html" (tagCloudCtx <> tagCtx <> postsCtx <> siteCtx)
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

tagCtx :: Tags -> Context String
tagCtx tags = tagsField "tags" tags <> defaultContext

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

-- | 記事の一覧
postList :: Compiler [Item String]
postList = loadAll ("posts/*" .&&. hasVersion "postList") >>= recentFirst

-- | 最近の記事一覧フィールド
recentPostsField :: Int -> Context String -> Compiler (Context b)
recentPostsField n ctx = listField "recent_posts" ctx . return . take n <$> postList

-- | すべての記事一覧フィールド
allPostsField :: Context String -> Compiler (Context b)
allPostsField ctx = listField "posts" ctx . return <$> postList

--------------------------------------------------------------------------------

tagListField :: String -> Tags -> Context a
tagListField key tags = field key $ \_ -> renderTagList' tags
  where
    renderTagList' = renderTags makeLink unlines
    makeLink tag url count _ _ =
        renderHtml $ H.li $ H.a ! A.href (toValue url) $ toHtml $ tag ++ " (" ++ show count ++ ")"
