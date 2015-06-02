# RMarkdown to Jekyll

# Code downloaded from http://gtog.github.io/workflow/2013/06/12/rmarkdown-to-rbloggers/

# For R-bloggers who want to post from Jekyll, you only need to do the following:

# 1) Create your R Markdown document with embedded code and graphics.
# 2) Create an new R Script and call it whatever you like.
# 3) Minimally, this script should contain the following code:


myjekyllsite = c("http://danilofreire.github.io/")
KnitPost <- function(input, base.url = "myjekyllsite") {
        require(knitr)
        opts_knit$set(base.url = base.url)
        fig.path <- paste0("figs/", sub(".Rmd$", "", basename(input)), "/")
        opts_chunk$set(fig.path = fig.path)
        opts_chunk$set(fig.cap = "center")
        render_jekyll()
        knit(input, envir = parent.frame())
}


# 4) Type: KnitPost("YOURNEWPOST.Rmd")
# 5) Move the resulting image folder (called ‘figs’) to your local git repository for your Jekyll site.
# 6) Move the resulting markdown file to your local posts folder for your Jekyll site.
# 7) Add "layout: post" to the header. For instance:
# ---
# layout: post
# title:  "This is a blog post"
# date:   2014-12-24 14:43
# categories: blogs
# tags: R
# ---
# 8) Note: You need to set the “base url” in the code to the full blog site URL or the Feedburner feed won’t
# pick up the images in the “fig” folder.
# 9) To render equations with MathJax, use the following code after the header (thanks to fommil):

# <style TYPE="text/css">
# code.has-jax {font: inherit; font-size: 100%; background: inherit; border: inherit;}
# </style>
# <script type="text/x-mathjax-config">
# MathJax.Hub.Config({
#     tex2jax: {
#         inlineMath: [['$','$'], ['\\(','\\)']],
#         skipTags: ['script', 'noscript', 'style', 'textarea', 'pre'] // removed 'code' entry
#     }
# });
# MathJax.Hub.Queue(function() {
#     var all = MathJax.Hub.getAllJax(), i;
#     for(i = 0; i < all.length; i += 1) {
#         all[i].SourceElement().parentNode.className += ' has-jax';
#     }
# });
# </script>
# <script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
