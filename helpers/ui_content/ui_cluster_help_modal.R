# ui_cluster_help_modal.R
# Help modal for Clustering tab

# Observer for help button
observeEvent(input$show_cluster_help, {
  showModal(modalDialog(
    title = tr("cluster.help.modal.title"),
    size = "l",
    easyClose = TRUE,
    footer = modalButton(tr("cluster.help.close")),

    tagList(
      # === OVERVIEW ===
      div(class = "seriarc-panel",
        h4(tr("cluster.help.overview.title"), class = "mt-0"),
        p(tr("cluster.help.overview.desc")),
        tags$ul(
          tags$li(tags$strong(tr("cluster.help.kmeans")), " ", tr("cluster.help.kmeans.desc")),
          tags$li(tags$strong(tr("cluster.help.hierarchical")), " ", tr("cluster.help.hierarchical.desc")),
          tags$li(tags$strong(tr("cluster.help.fuzzy")), " ", tr("cluster.help.fuzzy.desc")),
          tags$li(tags$strong(tr("cluster.help.gmm")), " ", tr("cluster.help.gmm.desc"))
        ),
        tags$hr()
      ),
      
      # === METHOD COMPARISON ===
      div(class = "seriarc-panel",
        h4(tr("cluster.help.comparison.title"), class = "mt-0"),

        # K-Means
        div(class = "well well-sm",
          h5(tags$strong("K-Means (Standard)")),
          p(tags$strong(tr("cluster.help.principle")), " ", tr("cluster.help.kmeans.principle")),
          p(tags$strong(tr("cluster.help.advantages")),
            tags$ul(
              tags$li(tr("cluster.help.kmeans.adv1")),
              tags$li(tr("cluster.help.kmeans.adv2")),
              tags$li(tr("cluster.help.kmeans.adv3")),
              tags$li(tr("cluster.help.kmeans.adv4"))
            )
          ),
          p(tags$strong(tr("cluster.help.disadvantages")),
            tags$ul(
              tags$li(tr("cluster.help.kmeans.disadv1")),
              tags$li(tr("cluster.help.kmeans.disadv2")),
              tags$li(tr("cluster.help.kmeans.disadv3"))
            )
          ),
          p(tags$strong(tr("cluster.help.recommendation")), tags$em(tr("cluster.help.kmeans.rec"))),
          p(tags$strong(tr("cluster.help.parameters")),
            tags$ul(
              tags$li(tags$code("nstart=50"), " - ", tr("cluster.help.kmeans.param1")),
              tags$li(tags$code("iter.max=200"), " - ", tr("cluster.help.kmeans.param2")),
              tags$li(tags$strong(tr("cluster.help.kmeans.param3")), " ", tr("cluster.help.kmeans.param3.desc"))
            )
          )
        ),

        # Hierarchisches Clustering
        div(class = "well well-sm",
          h5(tags$strong(tr("cluster.help.hclust.title"))),
          p(tags$strong(tr("cluster.help.principle")), " ", tr("cluster.help.hclust.principle")),
          p(tags$strong(tr("cluster.help.advantages")),
            tags$ul(
              tags$li(tr("cluster.help.hclust.adv1")),
              tags$li(tr("cluster.help.hclust.adv2")),
              tags$li(tr("cluster.help.hclust.adv3")),
              tags$li(tr("cluster.help.hclust.adv4"))
            )
          ),
          p(tags$strong(tr("cluster.help.disadvantages")),
            tags$ul(
              tags$li(tr("cluster.help.hclust.disadv1")),
              tags$li(tr("cluster.help.hclust.disadv2")),
              tags$li(tr("cluster.help.hclust.disadv3"))
            )
          ),
          p(tags$strong(tr("cluster.help.recommendation")), tags$em(tr("cluster.help.hclust.rec"))),
          p(tags$strong(tr("cluster.help.linkage")),
            tags$ul(
              tags$li(tags$strong("Ward.D2"), " ", tr("cluster.help.hclust.linkage.ward")),
              tags$li(tags$strong("Complete"), " - ", tr("cluster.help.hclust.linkage.complete")),
              tags$li(tags$strong("Average"), " - ", tr("cluster.help.hclust.linkage.average")),
              tags$li(tags$strong("Single"), " - ", tr("cluster.help.hclust.linkage.single"))
            )
          )
        ),

        # Fuzzy K-Means
        div(class = "well well-sm",
          h5(tags$strong(tr("cluster.help.fuzzy.title"))),
          p(tags$strong(tr("cluster.help.principle")), " ", tr("cluster.help.fuzzy.principle")),
          p(tags$strong(tr("cluster.help.advantages")),
            tags$ul(
              tags$li(tr("cluster.help.fuzzy.adv1")),
              tags$li(tr("cluster.help.fuzzy.adv2")),
              tags$li(tr("cluster.help.fuzzy.adv3")),
              tags$li(tr("cluster.help.fuzzy.adv4"))
            )
          ),
          p(tags$strong(tr("cluster.help.disadvantages")),
            tags$ul(
              tags$li(tr("cluster.help.fuzzy.disadv1")),
              tags$li(tr("cluster.help.fuzzy.disadv2")),
              tags$li(tr("cluster.help.fuzzy.disadv3"))
            )
          ),
          p(tags$strong(tr("cluster.help.recommendation")), tags$em(tr("cluster.help.fuzzy.rec"))),
          p(tags$strong(tr("cluster.help.fuzzy.param")),
            tags$ul(
              tags$li(tags$strong("m=1.1-1.5:"), " ", tr("cluster.help.fuzzy.m.low")),
              tags$li(tags$strong("m=2 (Standard):"), " ", tr("cluster.help.fuzzy.m.std")),
              tags$li(tags$strong("m>2:"), " ", tr("cluster.help.fuzzy.m.high"))
            )
          )
        ),

        # GMM
        div(class = "well well-sm",
          h5(tags$strong(tr("cluster.help.gmm.title"))),
          p(tags$strong(tr("cluster.help.principle")), " ", tr("cluster.help.gmm.principle2")),
          p(tags$strong(tr("cluster.help.advantages")),
            tags$ul(
              tags$li(tr("cluster.help.gmm.adv1")),
              tags$li(tr("cluster.help.gmm.adv2")),
              tags$li(tr("cluster.help.gmm.adv3")),
              tags$li(tr("cluster.help.gmm.adv4b"))
            )
          ),
          p(tags$strong(tr("cluster.help.disadvantages")),
            tags$ul(
              tags$li(tr("cluster.help.gmm.disadv1")),
              tags$li(tr("cluster.help.gmm.disadv2")),
              tags$li(tr("cluster.help.gmm.disadv3")),
              tags$li(tr("cluster.help.gmm.disadv4"))
            )
          ),
          p(tags$strong(tr("cluster.help.recommendation")), tags$em(tr("cluster.help.gmm.rec"))),
          p(tags$strong(tr("cluster.help.gmm.cov")),
            tags$ul(
              tags$li(tags$strong("VVV"), " - ", tr("cluster.help.gmm.vvv.desc")),
              tags$li(tags$strong("EEE"), " - ", tr("cluster.help.gmm.eee.desc")),
              tags$li(tags$strong("VEV"), " - ", tr("cluster.help.gmm.vev.desc")),
              tags$li(tags$strong("Auto (BIC)"), " - ", tr("cluster.help.gmm.auto.desc"))
            )
          )
        ),

        tags$hr()
      ),
      
      # === GENERAL SETTINGS ===
      div(class = "seriarc-panel",
        h4(tr("cluster.help.settings.title"), class = "mt-0"),

        tags$dl(
          tags$dt(tags$strong(tr("cluster.help.settings.what"))),
          tags$dd(
            tags$ul(
              tags$li(tags$strong(tr("cluster.help.settings.both")), " ", tr("cluster.help.settings.both.desc")),
              tags$li(tags$strong(tr("cluster.help.settings.sites")), " ", tr("cluster.help.settings.sites.desc"), " → ", tags$em(tr("cluster.help.settings.sites.available"))),
              tags$li(tags$strong(tr("cluster.help.settings.types")), " ", tr("cluster.help.settings.types.desc"))
            )
          ),

          tags$dt(tags$strong(tr("cluster.help.settings.k"))),
          tags$dd(
            tr("cluster.help.settings.k.desc"), " ", tags$strong(tr("cluster.help.settings.k.auto")),
            " ", tr("cluster.help.settings.k.desc2")
          ),

          tags$dt(tags$strong(tr("cluster.help.settings.dims"))),
          tags$dd(
            tags$ul(
              tags$li(tags$strong("2D:"), " ", tr("cluster.help.settings.2d")),
              tags$li(tags$strong("3D-5D:"), " ", tr("cluster.help.settings.3d")),
              tags$li(tags$em(tr("cluster.help.settings.dims.rec")))
            )
          ),

          tags$dt(tags$strong(tr("cluster.help.settings.seed"))),
          tags$dd(
            tr("cluster.help.settings.seed.desc"), " ",
            tr("cluster.help.settings.seed.desc2"),
            tags$br(),
            tags$em(tr("cluster.help.settings.seed.default"))
          ),

          tags$dt(tags$strong(tr("cluster.help.settings.ellipses"))),
          tags$dd(
            tr("cluster.help.settings.ellipses.desc")
          )
        ),

        tags$hr()
      ),
      
      # === QUALITY METRICS ===
      div(class = "seriarc-panel",
        h4(tr("cluster.help.quality.title"), class = "mt-0"),
        p(tr("cluster.help.quality.desc")),

        tags$dl(
          tags$dt(tags$strong(tr("cluster.help.quality.silhouette"))),
          tags$dd(
            tr("cluster.help.quality.silhouette.desc"),
            tags$br(),
            tags$strong(">0.7:"), " ", tr("cluster.help.quality.silhouette.strong"), " | ",
            tags$strong("0.5-0.7:"), " ", tr("cluster.help.quality.silhouette.moderate"), " | ",
            tags$strong("<0.5:"), " ", tr("cluster.help.quality.silhouette.weak"),
            tags$br(),
            tags$em(tr("cluster.help.quality.silhouette.rec"))
          ),

          tags$dt(tags$strong(tr("cluster.help.quality.ch"))),
          tags$dd(
            tr("cluster.help.quality.ch.desc"),
            tags$br(),
            tags$strong(">100:"), " ", tr("cluster.help.quality.ch.good"), " | ",
            tags$strong("50-100:"), " ", tr("cluster.help.quality.ch.moderate"), " | ",
            tags$strong("<50:"), " ", tr("cluster.help.quality.ch.weak")
          ),

          tags$dt(tags$strong(tr("cluster.help.quality.db"))),
          tags$dd(
            tr("cluster.help.quality.db.desc"),
            tags$br(),
            tags$strong("<0.5:"), " ", tr("cluster.help.quality.db.excellent"), " | ",
            tags$strong("0.5-1.0:"), " ", tr("cluster.help.quality.db.acceptable"), " | ",
            tags$strong(">1.0:"), " ", tr("cluster.help.quality.db.overlap")
          ),

          tags$dt(tags$strong(tr("cluster.help.quality.bss"))),
          tags$dd(
            tr("cluster.help.quality.bss.desc"), " ",
            tags$em(tr("cluster.help.quality.bss.note")), " ", tr("cluster.help.quality.bss.typical")
          )
        ),

        div(class = "alert alert-warning", role = "alert",
          tags$strong(tr("cluster.help.quality.warning")), " ", tr("cluster.help.quality.warning.desc")
        ),

        tags$hr()
      ),
      
      # === LEITTYPEN-CHARAKTERISIERUNG ===
      div(class = "seriarc-panel",
        h4(tr("cluster.help.indextype.title"), class = "mt-0"),
        p(tr("cluster.help.indextype.only")),
        p(tr("cluster.help.indextype.desc")),

        tags$ol(
          tags$li(tags$strong(tr("cluster.help.indextype.step1")), " ", tr("cluster.help.indextype.step1.desc")),
          tags$li(tags$strong(tr("cluster.help.indextype.step2")), " ", tr("cluster.help.indextype.step2.desc")),
          tags$li(tags$strong(tr("cluster.help.indextype.step3")), " ", tr("cluster.help.indextype.step3.desc")),
          tags$li(tags$strong(tr("cluster.help.indextype.step4")), " ", tr("cluster.help.indextype.step4.desc"))
        ),

        p(tags$strong(tr("cluster.help.indextype.n"))),
        tags$ul(
          tags$li(tags$strong(tr("cluster.help.indextype.n.desc"))),
          tags$li(tags$em(tr("cluster.help.indextype.n.example")))
        ),

        p(tags$strong(tr("cluster.help.indextype.criteria"))),
        tags$ul(
          tags$li(tr("cluster.help.indextype.crit1")),
          tags$li(tr("cluster.help.indextype.crit2")),
          tags$li(tr("cluster.help.indextype.crit3"))
        ),

        div(class = "alert alert-info", role = "alert",
          tags$strong(tr("cluster.help.indextype.tip")), " ", tr("cluster.help.indextype.tip.desc")
        ),

        tags$hr()
      ),
      
      # === VERWENDETE PAKETE ===
      div(class = "seriarc-panel",
        h4(tr("cluster.help.packages.title"), class = "mt-0"),

        tags$dl(
          tags$dt(tags$strong("stats::kmeans")),
          tags$dd(
            tr("cluster.help.packages.kmeans"), " R Core Team (2024). R: A Language and Environment for Statistical Computing. ",
            tags$br(),
            tags$em("Hartigan, J. A. & Wong, M. A. (1979). Algorithm AS 136: A K-Means Clustering Algorithm. Applied Statistics, 28, 100-108.")
          ),

          tags$dt(tags$strong("stats::hclust")),
          tags$dd(
            tr("cluster.help.packages.hclust"), " R Core Team (2024).",
            tags$br(),
            tags$em("Murtagh, F. & Legendre, P. (2014). Ward's Hierarchical Agglomerative Clustering Method. Journal of Classification, 31, 274-295.")
          ),

          tags$dt(tags$strong("cluster::fanny")),
          tags$dd(
            tr("cluster.help.packages.fanny"), " Maechler, M., Rousseeuw, P., et al. (2023). cluster: Cluster Analysis Basics and Extensions. R package.",
            tags$br(),
            tags$em("Kaufman, L. & Rousseeuw, P. J. (1990). Finding Groups in Data: An Introduction to Cluster Analysis. Wiley.")
          ),

          tags$dt(tags$strong("mclust::Mclust")),
          tags$dd(
            tr("cluster.help.packages.mclust"), " Scrucca, L., Fop, M., Murphy, T. B., & Raftery, A. E. (2016). mclust 5: Clustering, Classification and Density Estimation Using Gaussian Finite Mixture Models. The R Journal, 8(1), 289-317.",
            tags$br(),
            tags$em("Fraley, C. & Raftery, A. E. (2002). Model-Based Clustering, Discriminant Analysis, and Density Estimation. JASA, 97(458), 611-631.")
          ),

          tags$dt(tags$strong("cluster::silhouette")),
          tags$dd(
            tr("cluster.help.packages.silhouette"), " Maechler, M., et al. (2023).",
            tags$br(),
            tags$em("Rousseeuw, P. J. (1987). Silhouettes: A Graphical Aid to the Interpretation of Cluster Analysis. J. Comp. Appl. Math., 20, 53-65.")
          ),

          tags$dt(tags$strong("cluster::clusGap")),
          tags$dd(
            tr("cluster.help.packages.clusgap"), " Maechler, M., et al. (2023).",
            tags$br(),
            tags$em("Tibshirani, R., Walther, G., & Hastie, T. (2001). Estimating the Number of Clusters via the Gap Statistic. JRSS-B, 63(2), 411-423.")
          )
        ),

        tags$hr()
      ),
      
      # === RECOMMENDATIONS FOR PUBLICATIONS ===
      div(class = "seriarc-panel",
        h4(tr("cluster.help.pub.title"), class = "mt-0"),

        tags$ol(
          tags$li(tags$strong(tr("cluster.help.pub.repro")),
            tags$ul(
              tags$li(tr("cluster.help.pub.repro1")),
              tags$li(tr("cluster.help.pub.repro2")),
              tags$li(tr("cluster.help.pub.repro3"))
            )
          ),

          tags$li(tags$strong(tr("cluster.help.pub.method")),
            tags$ul(
              tags$li(tr("cluster.help.pub.method1")),
              tags$li(tr("cluster.help.pub.method2")),
              tags$li(tr("cluster.help.pub.method3"))
            )
          ),

          tags$li(tags$strong(tr("cluster.help.pub.interp")),
            tags$ul(
              tags$li(tr("cluster.help.pub.interp1")),
              tags$li(tr("cluster.help.pub.interp2")),
              tags$li(tr("cluster.help.pub.interp3"))
            )
          ),

          tags$li(tags$strong(tr("cluster.help.pub.viz")),
            tags$ul(
              tags$li(tr("cluster.help.pub.viz1")),
              tags$li(tr("cluster.help.pub.viz2")),
              tags$li(tr("cluster.help.pub.viz3"))
            )
          )
        ),

        div(class = "alert alert-success", role = "alert",
          tags$strong(tr("cluster.help.pub.best")), " ", tr("cluster.help.pub.best.desc")
        )
      )
    )
  ))
})
