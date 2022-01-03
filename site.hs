--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate, intersperse)
import Hakyll hiding (tagsField)
import qualified Skylighting
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- タグのリスト
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

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

    -- タグページ生成
    tagsRules tags $ \tag pat -> do
        route idRoute
        compile $ do
            taggedPosts <- recentFirst =<< loadAll pat
            posts <- postList
            let ctx_title = constField "title" $ "Posts tagged \"" ++ tag ++ "\""
                ctx_recent_posts = recentPostsField 5 defaultContext posts
                ctx_taglist = tagListField tags
                ctx_posts = postListField (postDateCtx <> defaultContext) taggedPosts
                ctx = ctx_title <> ctx_taglist <> ctx_recent_posts <> ctx_posts <> siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "layouts/post-list.html" ctx
                >>= loadAndApplyTemplate "layouts/default.html" ctx
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
            posts <- postList
            let ctx_recent_posts = recentPostsField 5 defaultContext posts
                ctx_taglist = tagListField tags
                ctx_tags = tagsField tags
                ctx = ctx_taglist <> ctx_tags <> ctx_recent_posts <> siteCtx

            pandocCompiler
                >>= loadAndApplyTemplate "layouts/post.html" ctx
                >>= loadAndApplyTemplate "layouts/default.html" ctx
                >>= relativizeUrls

    -- 過去記事リストの生成
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- postList
            let ctx_title = constField "title" "過去記事の一覧"
                ctx_recent_posts = recentPostsField 5 defaultContext posts
                ctx_taglist = tagListField tags
                ctx_posts = postListField (postDateCtx <> defaultContext) posts
                ctx = ctx_title <> ctx_taglist <> ctx_recent_posts <> ctx_posts <> siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "layouts/post-list.html" ctx
                >>= loadAndApplyTemplate "layouts/default.html" ctx
                >>= relativizeUrls

    -- トップページの生成
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- postList
            let ctx_recent_posts = recentPostsField 5 (tagsField tags <> teaserField "teaser" "content" <> defaultContext) posts
                ctx_taglist = tagListField tags
                ctx = ctx_taglist <> ctx_recent_posts <> siteCtx

            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "layouts/default.html" ctx
                >>= relativizeUrls

    -- その他ページの作成
    match (fromList ["about.html", "links.html"]) $ do
        route idRoute
        compile $ do
            posts <- postList
            let ctx_recent_posts = recentPostsField 5 defaultContext posts
                ctx_taglist = tagListField tags
                ctx = ctx_taglist <> ctx_recent_posts <> siteCtx

            getResourceBody
                >>= loadAndApplyTemplate "layouts/default.html" ctx
                >>= relativizeUrls

--------------------------------------------------------------------------------

seqconcat :: (Monoid a, Monad m) => [m a] -> m a
seqconcat xs = mconcat <$> sequence xs

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

-- | 記事の一覧
postList :: Compiler [Item String]
postList = loadAll ("posts/*" .&&. hasVersion "postList") >>= recentFirst

-- | 最近の記事一覧フィールド
recentPostsField :: Int -> Context String -> [Item String] -> Context b
recentPostsField n ctx = listField "recent_posts" ctx . pure . take n

-- | すべての記事一覧フィールド
postListField :: Context String -> [Item String] -> Context b
postListField ctx = listField "posts" ctx . pure

--------------------------------------------------------------------------------

-- | Hakyll.Web.Tags.tagsField の、分割文字を" "とした版。
tagsField :: Tags -> Context a
tagsField = tagsFieldWith getTags simpleRenderLink (mconcat . intersperse " ") "tags"
  where
    simpleRenderLink :: String -> Maybe FilePath -> Maybe H.Html
    simpleRenderLink _ Nothing = Nothing
    simpleRenderLink tag (Just filePath) =
        Just $
            H.a ! A.title (H.stringValue ("All pages tagged '" ++ tag ++ "'."))
                ! A.href (toValue $ toUrl filePath)
                $ toHtml tag

-- | タグリストのフィールド (各タグを<li>で囲む)
tagListField :: Tags -> Context a
tagListField tags = field "taglist" $ \_ -> renderTagList' tags
  where
    renderTagList' = renderTags makeLink unlines
    makeLink tag url count _ _ =
        renderHtml $ H.li $ H.a ! A.href (toValue url) $ toHtml $ tag ++ " (" ++ show count ++ ")"
