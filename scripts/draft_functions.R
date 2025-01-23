suppressPackageStartupMessages({
  library(magrittr)
  library(tidyverse)
  # library(ggtree)
  #	library(phytools)
})


tree = function(clade=NULL,
                collapse=NULL,
                highlight=NULL,
                # highlight = list(dromaeosauridae = 'green'),
                # timescale='ages',
                auto_order=FALSE) {

  # stopifnot(timescale %in% c('periods', 'epochs', 'ages'))

  # clade=NULL ; collapse=NULL ; highlight=NULL ; timescale='epochs'

  # load data
  tips = load_tips()
  nodes = load_nodes()
  geotime = load_geotime()
  extinctions = load_extinctions()
  events = load_events()

  # data for plotting
  df = dplyr::bind_rows(tips, nodes) %>%
    dplyr::mutate(col = name %>% stringr::str_detect('\\*$') %>% if_else('red', 'black')) %>%
    dplyr::mutate(name = name %>% str_remove('\\*$')) %>%
    dplyr::mutate(col = col %>% replace_na('black')) %>%
    check_tree_data %>%
    get_clade(clade) %>%
    collapse_clades(collapse) %>%
    add_depths() %>%
    arrange_tree()
    # dplyr::arrange(desc(depth), desc(from)) %>%
    # dplyr::mutate(index = nrow(.):1) %>%
    # reindex_tree_data()

  # filter geotime
  geotime$periods %<>%
    filter(to > min(df$from, na.rm = TRUE))
  geotime$epochs %<>%
    filter(period %in% geotime$periods$period)
  geotime$ages %<>%
    filter(period %in% geotime$periods$period)

  # filter extinctions and events
  extinctions %<>%
    filter(ma > min(geotime$periods$from))
  events %<>%
    filter(ma > min(geotime$periods$from))

  # x axis ticks
  x_breaks = rev(seq(0, min(geotime$period$from), -25))

  if (auto_order) {
    df = order_tree_data(df)
  }

  # base plot and theme
  plt = df %>%
    ggplot(aes(x=from, y=index)) +
    # geom_segment(
    #   mapping=aes(xend=to, yend=index),
    #   data=df %>% filter(is_tip)
    # ) +
    # geom_text(
    #   aes(x=(from + to) / 2, label=name),
    #   data=df %>% filter(is_tip),
    #   vjust=-0.5,
    #   size=3
    # ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.line.x=element_line(),
      legend.position = "none"
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      labels = -x_breaks,
      # expand = c(left, ?, right, ?)
      expand = c(0.001, 0, 0.001, 0),
    ) +
    scale_y_continuous(
      breaks = 0:sum(df$is_tip),
      labels = 0:sum(df$is_tip),
      # expand = c(bottom, ?, top, ?)
      expand = c(0, 0, 0.05, 0),
    )

  index_min = min(df$index, na.rm=TRUE)
  index_max = max(df$index, na.rm=TRUE)
  index_range = abs(index_max - index_min) + 1

  h = index_range * 0.05

  y_max = index_max + 0.5 #+ h / 2
  y_min = 0.5

  x_min = min(geotime$periods$from)
  x_max = max(geotime$periods$to)

  ages_max = y_min
  ages_min = ages_max - h * 2
  epochs_max = ages_min
  epochs_min = epochs_max - h
  periods_max = epochs_min
  periods_min = periods_max - h

  plt = plt +
    geom_hline(yintercept = y_min, col='grey50', lwd=.5) +
    geom_hline(yintercept = epochs_max, col='grey50', lwd=.25) +
    geom_hline(yintercept = periods_max, col='grey50', lwd=.5) +
    geom_hline(yintercept = periods_min, col='grey50', lwd=.5)

  # periods
  plt = plt +
    geom_rect(
      mapping=aes(xmin=from, xmax=to), fill=NA,
      ymin=periods_min, ymax=y_max, color='grey50', lwd=.5,
      data=geotime$periods,
      inherit.aes = FALSE
    ) +
    geom_rect(
      mapping=aes(xmin=from, xmax=to), fill=geotime$periods$fill, alpha=0.25,
      ymin=periods_min, ymax=periods_max, color=NA,
      data=geotime$periods,
      inherit.aes = FALSE
    ) +
    geom_text(
      aes(x=(from + to) / 2, label=period),
      y = (periods_max + periods_min) / 2,
      size=4,
      data=geotime$periods,
      inherit.aes = FALSE
    )

  # epochs
  plt = plt +
    geom_rect(
      mapping=aes(xmin=from, xmax=to), fill=NA,
      ymin=epochs_min, ymax=y_max, color="grey50", lwd=0.25,
      data=geotime$epochs,
      inherit.aes = FALSE
    ) +
    geom_rect(
      mapping=aes(xmin=from, xmax=to), fill=geotime$epochs$fill, alpha=0.25,
      ymin=epochs_min, ymax=y_max, color=NA,
      data=geotime$epochs,
      inherit.aes = FALSE
    ) +
    geom_text(
      aes(x=(from + to) / 2, label=epoch),
      y = (epochs_max + epochs_min) / 2,
      size=3,
      data=geotime$epochs %>% filter(!is.na(epoch)),
      inherit.aes = FALSE
    )

  # ages
  plt = plt +
    geom_rect(
      mapping=aes(xmin=from, xmax=to), fill=NA,
      data=geotime$ages,
      ymin=ages_min, ymax=ages_max, color="grey75", lwd=0.1,
      inherit.aes = FALSE
    ) +
    geom_text(
      mapping=aes(x=(from + to) / 2, label=age),
      data=geotime$ages %>% filter(!is.na(age)),
      y = ages_min + (ages_max - ages_min) * 0.05,
      hjust=0,
      # vjust=0,
      size=2.5,
      angle=90,
      inherit.aes = FALSE
    )

  # extinctions
  plt = plt +
    geom_segment(
      mapping=aes(x=ma, xend=ma),
      data=extinctions,
      y=y_min, yend=y_max,
      lty=1, lwd=0.5, col='red',
      inherit.aes = FALSE
    ) +
    geom_text(
      mapping=aes(x=ma, label=label),
      data=extinctions,
      y=y_max,
      col='grey50',
      vjust = -0.5,
      inherit.aes = FALSE
    )

  # events
  plt = plt +
    geom_segment(
      mapping=aes(x=ma, xend=ma),
      data=events,
      y=y_min, yend=y_max,
      lty=1, lwd=0.5, col='blue',
      inherit.aes = FALSE
    ) +
    geom_text(
      mapping=aes(x=ma, label=label),
      data=events,
      y=y_max,
      col='grey50',
      vjust = -0.5,
      inherit.aes = FALSE
    )

  # highlights
  for (i in seq_along(highlight)) {
    clade = names(highlight)[i]
    tmp = df %>% get_clade(clade)
    if (nrow(tmp) == 0) {
      warning("couldn't find ", clade, " in plot data")
      next
    }
    plt = plt +
      annotate(
        "rect",
        xmin=min(tmp$from) - 1,
        xmax=max(tmp$to, na.rm=TRUE) + 1,
        ymin=min(tmp$index) - 0.25,
        ymax=max(tmp$index) + 0.5,
        alpha=0.25,
        fill=highlight[[i]],
        col=NA,
      )
  }

  # connections
  for (i in 1:nrow(df)) { # i=1
    if (df$is_tip[i]) next
    children = df %>% filter(name %in% df$children[[i]])
    for (j in 1:nrow(children)) { # j=1
      vals = sigmoid(
        x1=df$from[i],
        x2=children$from[j],
        y1=df$index[i],
        y2=children$index[j]
      )
      plt = plt +
        geom_path(
          mapping=aes(x=x, y=y),
          data=vals,
          lty=1,
          col='grey60',
          inherit.aes = FALSE
        )
      # plt = plt +
      #   geom_segment(
      #     x=df$from[i],
      #     xend=children$from[j],
      #     y=df$index[i],
      #     yend=children$index[j],
      #     lty=1,
      #     col='grey60'
      #   )
    }
  }

  # tips
  tips = df %>% filter(is_tip)
  plt = plt +
    geom_segment(
      mapping=aes(xend=to, yend=index),
      data=tips
    ) +
    geom_text(
      aes(x=(from + to) / 2, label=name),
      data=tips,
      vjust=-0.5,
      size=3
    ) +
    geom_point(
      mapping=aes(x=to),
      data=tips %>% filter(to < 0),
      size=1
    ) +
    geom_point(
      data=tips,
      col=tips$col,
      size=1
    )

  # nodes
  nodes = df %>% filter(!is_tip)
  plt = plt +
    geom_point(
      # mapping=aes(x=from, y=index),
      data=nodes,
      col=nodes$col,
      size=1.5
    ) +
    geom_text(
      # mapping=aes(x=from, y=index, label=name),
      mapping=aes(label=name),
      data=nodes,
      # hjust=1.1,
      hjust=0.5,
      vjust=-0.5,
      size=3,
    )

  return(plt)

}

load_tips = function() {
  # todo: read from csv
  tribble(
    ~name                  , ~from, ~to,
    # theropods
    'staurikosaurus'       , 233.23, 228,
    'herrerasaurus'        , 231.4 , 228.91,
    'eodromaeus'           , 231.4 , 229, # 'dawn runner', Argentina
    # 'coelophysoidea'       , 227   , 183,
    'coelophysis'          , 215   , 208.5,
    'dilophosaurus'        , 195.2 , 183.7, # two-crested lizard
    'ceratosaurus'         , 153   , 148, # 'horn lizard', USA
    'noasauridae'          , 164   ,  66, #
    'abelisauridae'        , 143   ,  66, # 'Abel's lizards'
    'megalosauridae'       , 170   , 145, # incl. Megalosaurus, Torvosaurus
    'baryonyx'             , 130   , 125, # 'heavy claw', UK
    'spinosaurus'          , 100   ,  94, # 'spine lizard', North Africa
    'allosaurus'           , 155   , 143.1,
    'metriacanthosauridae' , 174   , 125, # 'moderately-spined lizards'
    'tyrannotitan'         , 113   , 100.5, # 'tyrant titan', Argentina
    'giganotosaurus'       ,  99.6 ,  95,   # 'giant southern lizard', Argentina
    'carcharodontosaurus'  , 100   ,  94,   # 'jagged toothed lizard', Northwest Africa
    # 'tyrannosauroidea'     , 166   ,  66,
    'tyrannosauridae'      ,  81.9 ,  66,
    'ornitholestes'        , 154   , 154,
    'compsognathidae'      , 151.5 , 108, # incl. Compsognathus, Sinosauropteryx
    # 'ornithomimosauria'    , 140   ,  66,
    'ornithomimidae'       ,  96   ,  66,
    'deinocheiridae'       , 115   ,  69,
    'alvarezsauroidea'     , 160   ,  66,
    'therizinosauria'      , 145   ,  66,
    'oviraptorosauria'     , 130   ,  66,
    'troodontidae'         , 150   ,  66, # 'wounding tooth', USA (Montana), incl. Troodon
    'deinonychus'          , 115   , 108,
    'velociraptor'         ,  75   ,  71,
    'scansoriopterygidae'  , 165   , 156, # 'climbing wings', China, incl. Scansoriopteryx, Epidexipteryx
    'microraptor'          , 125   , 120,
    'archaeopteryx'        , 150.8 , 148.5, # 'old-wing', Southern Germany
    'enantiornithes'       , 136   ,  66, # 'opposite birds'
    'ichthyornis'          ,  95   ,  83.5,
    'hesperornis'          ,  83.6 ,  72,

    # aves
    'galloanserae'         , 72    ,   0, # maastrichtian
    'phorusrhacidae'       ,  43   ,   0.1, # terror bids
    'rheidae'              ,  56   ,   0,
    'casuariiformes'       ,  23   ,   0, # cassowarys and emus
    'apterygidae'          ,  23   ,   0, # kiwis
    'struthionidae'        ,  21   ,   0,
    'dinornithiformes'     ,  17   , 0.0006, # moas, incl Dinornis
    'aepyornithiformes'    , 2.58  , 0.001, #  elephant birds, incl Aepyornis

    # sauropodomorpha
    'massospondylidae'   , 227   , 176,
    'plateosauridae'     , 225   , 190,
    # 'sauropoda'          , 228   ,  66,
    'vulcanodontidae'    , 199   , 175,
    'mamenchisauridae'   , 184.5 , 114,
    'cetiosauridae'      , 178   , 161,
    'diplodocidae'       , 170   , 136.4,
    'brachiosauridae'    , 160   , 100,
    'camarasauridae'     , 155   , 145,
    'euhelopodidae'      , 145   ,  88,
    'titanosauria'       , 140   ,  66,

    'heterodontosauridae', 201   , 140,
    'scelidosaurus'      , 196.5 , 183,
    'stegosauria'        , 169   , 100.5,
    'nodosauridae'       , 155   ,  66, # incl. Polacanthus
    'ankylosauridae'     , 122   ,  66,
    # 'ornithopoda'        , 164   ,  66,
    'dryosauridae'       , 164   , 115,
    'hypsilophodontidae' , 130   , 125,
    'iguanodontidae'     , 126   , 122,
    'hadrosauridae'      ,  86   ,  66,
    # 'ceratopsia'         , 161   ,  66,
    'psittacosaurus'     , 125   , 105,
    'ceratopsidae'       ,  82   ,  66,
    'protoceratopsidae'  ,  75   ,  71,
    'pachycephalosauria' , 100   ,  66,

    # pterosauria
    'dimorphodontidae'   , 208   , 182,
    'rhamphorhynchidae'  , 182   , 148.5,
    'pterodactylus'      , 150.8 , 148.5, # winged finger
    'ornithocheiridae'   , 140   ,  90,
    'anhangueridae'      , 140   ,  92.5,
    'azhdarchidae'       ,  92   ,  66, # includes Quetzalcoatlus
    'pteranodontia'      ,  92   ,  66,

    # pseudosuchia
    'ctenosauriscidae'   , 247.5 , 237,   # all species had large "sails" on their backs
    'poposauridae'       , 237   , 201.3, # bipedal carnivores
    'shuvosauridae'      , 235   , 201.3, # theropod-like pseudosuchians
    'prestosuchidae'     , 247   , 225,
    'phytosauria'        , 242   , 201.4,
    'rauisuchia'         , 240   , 200,
    'aetosauria'         , 231.4 , 201.4,
    'saurosuchus'        , 231.4 , 225,
    'ornithosuchidae'    , 228   , 203.6,
    'sphenosuchia'       , 228   , 152,
    # 'thalattosuchia'     , 201   , 100,
    'teleosauridae'      , 183   , 145,
    'metriorhynchidae'   , 168   , 125,
    'sarcosuchus'        , 133   , 112,
    'sebecidae'          ,  67.6 , 11.8, # the latest surviving group of non-crocodilian crocodylomorphs
    'alligatoroidea*'    ,  78   ,   0,  # campanian
    'crocodyloidea*'     ,  70   ,   0,  # maastrichtian
    'gavialoidea*'       ,  70   ,   0,  # maastrichtian
    'rhynchosauria'      , 251.5 , 225,
    'ichthyosauromorpha' , 251.3 ,  90,
    'nothosauroidea'     , 251   , 210,
    'pachypleurosauria'  , 251   , 227,
    'placodontia'        , 245   , 201.3,
    'pistosaurus'        , 247.2 , 237,
    'pliosauridae'       , 228   ,  89.3, # incl. Liopleurodon (166–155) Pliosaurus (155.7–147) Kronosaurus (125–99.6)
    'plesiosauridae'     , 199.6 , 174.7,
    'leptocleididae'     , 145   , 100.5,
    'elasmosauridae'     , 130   ,  66,
    'thalattosauria'     , 247   , 201.4,
    'araeoscelidia'      , 302   , 275.6,
    'testudinata'        , 210   ,   0,
    'rhynchocephalia'    , 240   ,   0,
    'mosasauria'         , 121   ,  66,
    'gekkota'            , 100   ,   0,
    'varanidae'          ,  80   ,   0,
    'chamaeleonidae'     ,  26   ,   0,
    'snakes'             ,  94   ,   0,
    'mesosauria'         , 299   , 270.6,
    'pareiasauria'       , 265   , 252,

    # synapsids
    'ophiacodontidae'    , 308   , 273,
    'edaphosauridae'     , 307   , 272,
    'diadectidae'        , 305   , 256, # incl. diadectes = 'crosswise-biter' one of the first herbivorous tetrapods, one of the first fully terrestrial vertebrates to attain large size
    'sphenacodontidae'   , 300   , 272, # 'wedge point tooth family' includes Dimetrodon
    # 'dinocephalia'       , 279   , 260, # 'terrible head'
    'moschops'           , 265   , 260,
    'gorgonopsia'        , 270   , 252,
    'dicynodontia'       , 268   , 201.4, # 'two dog tooth' includes Lystrosaurus
    'therocephalia'      , 266   , 242, # 'beast head'
    'morganucodonta'     , 210   , 140,
    'multituberculata'   , 166   ,  40,
    'monotremata'        , 125   ,   0,
    'marsupalia'         ,  66   ,   0,
    'proboscidea'        ,  60   ,   0,
    'sirenia'            ,  55.8 ,   0,
    'cingulata'          ,  58.7 ,   0, # armadillos including Glyptodont
    'pilosa'             ,  55.8 ,   0, # sloths and anteaters
    'chiroptera'         ,  56   ,   0,
    'carnivora'          ,  51.88,   0,
    'hyaenodontidae'     ,  55.2 ,  16.9,
    'perissodactyla'     ,  56   ,   0, # Tapiridae, Brontotheriidae, Equidae, Chalicotheriidae, Rhinocerotidae, Paraceratheriidae
    'basilosauridae'     ,  43   ,  33.9,
    'mysticeti'          ,  56   ,   0,
    'odontoceti'         ,  33.9 ,   0,
    'andrewsarchus'      ,  47.8 ,  37.71,
    'entelodontidae'     ,  37.2 ,  15.97,
    'lagomorpha'         ,  59   ,   0,
    'rodentia'           ,  59   ,   0,
    'primates'           ,  65.9 ,   0,
    # 'seymouria*'         ,  295, NA,
    'seymouriamorpha'    , 305 , 295,
    # 'sauria', 268.5, 0,
    # 'paraceratherium', 34, 23,
    # 'chalicotherioidea', 48.6  , 0.005,
    # 'equidae', 55.8, 0,
    # 'phorusrhacidae', 43, 0.1,

    # 'lepospondyli'     , 339   , 251.902,
    'eryops'           , 299   , 273,
    'archegosauridae'  , 297   , 252, # includes Prionosuchus
    'gymnophiona'      , 223   ,   0, # caecilian
    'anura'            , 200   ,   0, # frogs and toads
    'urodela'          , 168   ,   0, # salamanders
    'tiktaalik'        , 375   , NA,
    'elginerpeton'     , 375   , NA,
    'ichthyostega'     , 365   , 360,
    'acanthostega'     , 365   , NA,
    # 'Ichthyostegalia'  , 368   , 329,
    'diplocaulus'      , 306   , 255,
    # 'acanthodii'       , Early Silurian–Permian
    # 'gnathostomata'    , 439   , 0,
    # 'chondrichthyes'   , 439   , 0, # cartilagenous fish
    # 'placodermi'       , 439   , 358.9,
    # # 'osteichthyes'     , 425   , 0, # bony fish
    # 'actinopterygii'   , 425   , 0, # ray-finned fish
    # # 'sarcopterygii'    , 425   , 0, # lobe-finned fish
    # 'elasmobranchii', 387.7, 0, # sharks, rays, skates, sawfish
    # 'vertebrata'     , 518, 0,
    # 'agnatha'        , 518, 0,
    # Selachii # modern sharks
  ) %>%
    dplyr::mutate(name = str_to_lower(name)) %>%
	dplyr::mutate(to = if_else(is.na(to), from, to)) %>%
    dplyr::mutate(across(c(from, to), ~-abs(.x))) %>%
    dplyr::mutate(is_tip = TRUE) %>%
    dplyr::mutate(index = nrow(.):1) %>%
    dplyr::select(index, everything())
}

load_nodes = function() {
  tribble(
    ~name              , ~from , ~children,
# 	'gnathostomata'    , 439   , c('placodermi', 'chondrichthyes', 'osteichthyes'),
#     'chondrichthyes'   , 439   , c('elasmobranchii'),
#     'osteichthyes'     , 425   , c('actinopterygii', 'sarcopterygii'),
# 	'sarcopterygii'    , 425   , c('tetrapodomorpha'),

    'aves*'            ,  75   , c('neognathae', 'palaeognathae'),
    'neognathae'       ,  72   , c('galloanserae', 'neoaves'),
    'neoaves'          , 62.5  , c('phorusrhacidae'),
    'palaeognathae'    ,  60   , c('rheidae', 'casuariiformes', 'apterygidae', 'struthionidae', 'dinornithiformes', 'aepyornithiformes'),

    'afrotheria'       ,  65   , c('proboscidea', 'sirenia'),
    'amniota'          , 323   , c('sauropsida', 'synapsida'),
    'ankylosauria'     , 167   , c('nodosauridae', 'ankylosauridae'),
    'anomodontia'      , 270   , c('dicynodontia'),
    'archelosauria'    , 260   , c('archosauromorpha', 'pantestudines'),
    'artiodactyla'     , 56    , c('cetacea', 'andrewsarchus', 'entelodontidae'),
    'archosauria'      , 248   , c('avemetatarsalia', 'pseudosuchia'),
    'archosauromorpha' , 260   , c('rhynchosauria', 'archosauria', 'ichthyosauromorpha', 'sauropterygia', 'thalattosauria'),
    # 'avemetatarsalia'  , 247   , c('dinosauromorpha', 'pterosauria'),
    'avemetatarsalia'  , 247   , c('dinosauria', 'pterosauria'),
    'cetacea'          ,  56   , c('basilosauridae', 'odontoceti', 'mysticeti'),
    'crocodylomorpha'  , 235   , c('sphenosuchia', 'crocodyliformes', 'thalattosuchia'),
    'crocodyliformes'  , 225   , c('crocodilia', 'sarcosuchus', 'sebecidae'),
    'crocodilia'       , 100   , c('alligatoroidea', 'crocodyloidea', 'gavialoidea'),
    'cynodontia'       , 260   , c('mammalia', 'morganucodonta'),
    # 'dinosauromorpha'  , 247   , c('dinosauria'),
    'dinosauria*'      , 240   , c('saurischia', 'ornithischia'),
    'euarchontoglires' ,  66   , c('lagomorpha', 'rodentia', 'primates'),
    'eureptilia'       , 311   , c('diapsida', 'araeoscelidia'),
    'diapsida'         , 302   , c('sauria'),
    # 'eutheria'        , 125   , c('placentalia'),
    'laurasiatheria'   ,  65   , c('chiroptera', 'carnivora', 'perissodactyla', 'artiodactyla', 'hyaenodontidae'),
    'lepidosauromorpha', 252   , c('squamata', 'rhynchocephalia'),
    'lizards'          , 168   , c('mosasauria', 'varanidae', 'gekkota', 'chamaeleonidae'),
    'mammalia'         , 225   , c('theria', 'monotremata', 'multituberculata'),
    'marginocephalia*' , 165   , c('ceratopsia', 'pachycephalosauria'),
    'ceratopsia'       , 161   , c('psittacosaurus', 'ceratopsidae', 'protoceratopsidae'),
    # 'metatheria'      , 125   , c('marsupalia'),
    'ornithischia*'    , 235   , c('thyreophora', 'heterodontosauridae', 'ornithopoda', 'marginocephalia'),
    'thyreophora'      , 201   , c('scelidosaurus', 'ankylosauria', 'stegosauria'),
    'ornithocheiroidea', 145   , c('ornithocheiridae', 'azhdarchidae', 'pteranodontia', 'anhangueridae'),
    'ornithopoda*'     , 170   , c('dryosauridae', 'hypsilophodontidae', 'iguanodontidae', 'hadrosauridae'),
    'pantestudines'    , 240   , c('testudinata'),
    'placentalia*'     ,  90   , c('xenarthra', 'afrotheria', 'laurasiatheria', 'euarchontoglires'),
    'parareptilia'     , 306   , c('pareiasauria', 'mesosauria'),
    'poposauroidea'    , 248   , c('poposauridae', 'shuvosauridae', 'ctenosauriscidae'),
    'pistosauroidea'   , 251   , c('pistosaurus', 'plesiosauria'),
    'plesiosauria*'    , 232   , c('pliosauridae', 'plesiosauridae', 'elasmosauridae', 'leptocleididae'),
    'pseudosuchia'     , 248   , c('saurosuchus', 'prestosuchidae', 'poposauroidea', 'ornithosuchidae', 'phytosauria', 'aetosauria', 'rauisuchia', 'crocodylomorpha'),
    'pterosauria'      , 228   , c('pterodactyloidea', 'dimorphodontidae', 'rhamphorhynchidae'),
    'pterodactyloidea' , 162.7 , c('pterodactylus', 'ornithocheiroidea'),
    'sauria'           , 265.8 , c('lepidosauromorpha', 'archelosauria'),
    'saurischia*'      , 237   , c('sauropodomorpha', 'theropoda'),
    'sauropodomorpha'  , 230   , c('massospondylidae', 'plateosauridae', 'sauropoda'),
    'sauropoda'        , 201   , c('vulcanodontidae', 'mamenchisauridae', 'cetiosauridae', 'diplodocidae', 'brachiosauridae', 'camarasauridae', 'euhelopodidae', 'titanosauria'),
    'sauropsida'       , 311   , c('parareptilia', 'eureptilia'),
    'sauropterygia*'   , 255   , c('nothosauroidea', 'pachypleurosauria', 'placodontia', 'pistosauroidea'),
    'sphenacodontia'   , 304   , c('sphenacodontidae', 'therapsida'),
    'squamata'         , 168   , c('lizards', 'snakes'),
    'synapsida'        , 318   , c('edaphosauridae', 'diadectidae', 'ophiacodontidae', 'sphenacodontia'),

    # therapods
    'theropoda*'       , 236   , c('herrerasauridae', 'eodromaeus', 'coelophysoidea', 'ceratosauria', 'tetanurae'),
    'coelophysoidea'   , 227   , c('dilophosaurus', 'coelophysis'),
    'herrerasauridae'  , 233.23, c('herrerasaurus', 'staurikosaurus'),
    'megalosauroidea*' , 175   , c('spinosauridae', 'megalosauridae'),
    'spinosauridae'    , 139   , c('spinosaurus', 'baryonyx'),
    'ceratosauria'     , 199.3 , c('ceratosaurus', 'noasauridae', 'abelisauridae'),
    'tetanurae*'       , 201   , c('megalosauroidea', 'carnosauria', 'coelurosauria'),
    'tetrapoda'        , 370   , c('temnospondyli', 'lepospondyli', 'reptiliomorpha'),
    # 'seymouriamorpha'  , 305   , c('seymouria'),
    'reptiliomorpha'   , 340   , c('amniota', 'seymouriamorpha'),
	'lepospondyli'     , 339   , c('diplocaulus'),
    'carnosauria*'     , 180   , c('allosauridae', 'metriacanthosauridae', 'carcharodontosauridae'),
    'allosauridae'     , 179.17, c('allosaurus'),
    'carcharodontosauridae',154, c('carcharodontosaurus', 'giganotosaurus', 'tyrannotitan'),
    'paraves*'         , 167   , c('scansoriopterygidae', 'deinonychosauria', 'avialae'),
    'deinonychosauria' , 167   , c('dromaeosauridae', 'troodontidae'),
    'maniraptora*'     , 167   , c('alvarezsauroidea', 'therizinosauria', 'oviraptorosauria', 'paraves'),
    'avialae'          , 150.8 , c('archaeopteryx', 'enantiornithes', 'euornithes'),
    'euornithes'       , 130.7 , c('aves', 'ichthyornis', 'hesperornis'),
    'coelurosauria*'   , 170   , c('ornitholestes', 'tyrannosauroidea', 'maniraptora', 'compsognathidae', 'ornithomimosauria'),
    'ornithomimosauria', 140   , c('ornithomimidae', 'deinocheiridae'),
    'dromaeosauridae'  , 145   , c('deinonychus', 'velociraptor', 'microraptor'),
    'tyrannosauroidea' , 166   , c('tyrannosauridae'),

    'thalattosuchia'   , 201   , c('teleosauridae', 'metriorhynchidae'),
    'therapsida'       , 279.5 , c('anomodontia', 'dinocephalia', 'gorgonopsia', 'cynodontia', 'therocephalia'),
    'dinocephalia'     , 279   , c('moschops'),
    'theria'           , 125   , c('placentalia', 'marsupalia'), # 125 = guess
    # 'theria'          , 125   , c('metatheria', 'eutheria'), # 125 = guess
    'xenarthra'        ,  60   , c('cingulata', 'pilosa'),

    'tetrapodomorpha'  , 409   , c('tetrapoda', 'stegocephalia'),
	'stegocephalia'    , 375   , c('tiktaalik', 'elginerpeton', 'ichthyostega', 'acanthostega'),
	'temnospondyli'    , 330   , c('eryops', 'archegosauridae', 'lissamphibia'),
    'lissamphibia'     , 250   , c('anura', 'urodela', 'gymnophiona'),
  ) %>%
    dplyr::mutate(name = str_to_lower(name)) %>%
    dplyr::mutate(from = -abs(from)) %>%
    dplyr::mutate(children = children %>% purrr::map(str_to_lower)) %>%
    dplyr::mutate(is_tip = FALSE) %>%
    # dplyr::mutate(col = name %>% stringr::str_detect('\\*$') %>% if_else('red', 'black')) %>%
    # dplyr::mutate(name = name %>% str_remove('\\*$')) %>%
    identity
}

load_geotime = function() {
  geotime = tribble(
    ~era       , ~period        , ~epoch         , ~age           , ~from   ,
    'cenozoic' , 'quaternary'   , 'holocene'     , NA_character_  ,   0.0117,
    'cenozoic' , 'quaternary'   , 'pleistocene'  , NA_character_  ,   2.58  ,
    'cenozoic' , 'neogene'      , 'pliocene'     , NA_character_  ,   5.333 ,
    'cenozoic' , 'neogene'      , 'miocene'      , 'messinian'    ,   7.246 ,
    'cenozoic' , 'neogene'      , 'miocene'      , 'tortonian'    ,  11.63  ,
    'cenozoic' , 'neogene'      , 'miocene'      , 'serravallian' ,  13.82  ,
    'cenozoic' , 'neogene'      , 'miocene'      , 'langhian'     ,  15.97  ,
    'cenozoic' , 'neogene'      , 'miocene'      , 'burdigalian'  ,  20.44  ,
    'cenozoic' , 'neogene'      , 'miocene'      , 'aquitanian'   ,  23.03  ,
    'cenozoic' , 'paleogene'    , 'oligocene'    , 'chattian'     ,  27.82  ,
    'cenozoic' , 'paleogene'    , 'oligocene'    , 'rupelian'     ,  33.9   ,
    'cenozoic' , 'paleogene'    , 'eocene'       , 'priabonian'   ,  37.71  ,
    'cenozoic' , 'paleogene'    , 'eocene'       , 'bartonian'    ,  41.2   ,
    'cenozoic' , 'paleogene'    , 'eocene'       , 'lutetian'     ,  47.8   ,
    'cenozoic' , 'paleogene'    , 'eocene'       , 'ypresian'     ,  56     ,
    'cenozoic' , 'paleogene'    , 'paleocene'    , 'thanetian'    ,  59.2   ,
    'cenozoic' , 'paleogene'    , 'paleocene'    , 'selandian'    ,  61.6   ,
    'cenozoic' , 'paleogene'    , 'paleocene'    , 'danian'       ,  66     ,
    'mesozoic' , 'cretaceous'   , 'late'         , 'maastrichtian',  72.1   ,
    'mesozoic' , 'cretaceous'   , 'late'         , 'campanian'    ,  83.6   ,
    'mesozoic' , 'cretaceous'   , 'late'         , 'santonian'    ,  86.3   ,
    'mesozoic' , 'cretaceous'   , 'late'         , 'coniacian'    ,  89.8   ,
    'mesozoic' , 'cretaceous'   , 'late'         , 'turonian'     ,  93.9   ,
    'mesozoic' , 'cretaceous'   , 'late'         , 'cenomanian'   , 100.5   ,
    'mesozoic' , 'cretaceous'   , 'early'        , 'albian'       , 113.0   ,
    'mesozoic' , 'cretaceous'   , 'early'        , 'aptian'       , 121.4   ,
    'mesozoic' , 'cretaceous'   , 'early'        , 'barremian'    , 125.77  ,
    'mesozoic' , 'cretaceous'   , 'early'        , 'hauterivian'  , 132.6   ,
    'mesozoic' , 'cretaceous'   , 'early'        , 'valanginian'  , 139.8   ,
    'mesozoic' , 'cretaceous'   , 'early'        , 'berriasian'   , 145     ,
    'mesozoic' , 'jurassic'     , 'late'         , 'tithonian'    , 149.2   ,
    'mesozoic' , 'jurassic'     , 'late'         , 'kimmeridgian' , 154.8   ,
    'mesozoic' , 'jurassic'     , 'late'         , 'oxfordian'    , 161.5   ,
    'mesozoic' , 'jurassic'     , 'middle'       , 'callovian'    , 165.3   ,
    'mesozoic' , 'jurassic'     , 'middle'       , 'bathonian'    , 168.2   ,
    'mesozoic' , 'jurassic'     , 'middle'       , 'bajocian'     , 170.9   ,
    'mesozoic' , 'jurassic'     , 'middle'       , 'aalenian'     , 174.7   ,
    'mesozoic' , 'jurassic'     , 'early'        , 'toarcian'     , 184.2   ,
    'mesozoic' , 'jurassic'     , 'early'        , 'pliensbachian', 192.9   ,
    'mesozoic' , 'jurassic'     , 'early'        , 'sinemurian'   , 199.5   ,
    'mesozoic' , 'jurassic'     , 'early'        , 'hettangian'   , 201.3   ,
    'mesozoic' , 'triassic'     , 'late'         , 'rhaetian'     , 205.7   ,
    'mesozoic' , 'triassic'     , 'late'         , 'norian'       , 227     ,
    'mesozoic' , 'triassic'     , 'late'         , 'carnian'      , 237     ,
    'mesozoic' , 'triassic'     , 'middle'       , 'ladinian'     , 242     ,
    'mesozoic' , 'triassic'     , 'middle'       , 'anisian'      , 247.2   ,
    'mesozoic' , 'triassic'     , 'early'        , 'olenekian'    , 251.2   ,
    'mesozoic' , 'triassic'     , 'early'        , 'induan'       , 251.902 ,
    'paleozoic', 'permian'      , 'lopingian'    , 'changhsingian', 254.14  ,
    'paleozoic', 'permian'      , 'lopingian'    , 'wuchiapingian', 259.51  ,
    'paleozoic', 'permian'      , 'guadalupian'  , 'capitanian'   , 264.28  ,
    'paleozoic', 'permian'      , 'guadalupian'  , 'wordian'      , 266.9   ,
    'paleozoic', 'permian'      , 'guadalupian'  , 'roadian'      , 273.01  ,
    'paleozoic', 'permian'      , 'cisuralian'   , 'kungurian'    , 283.5   ,
    'paleozoic', 'permian'      , 'cisuralian'   , 'artinskian'   , 290.1   ,
    'paleozoic', 'permian'      , 'cisuralian'   , 'sakmarian'    , 293.52  ,
    'paleozoic', 'permian'      , 'cisuralian'   , 'asselian'     , 298.9   ,
    'paleozoic', 'carboniferous', 'pennsylvanian', 'gzhelian'     , 303.7   ,
    'paleozoic', 'carboniferous', 'pennsylvanian', 'kasimovian'   , 307.0   ,
    'paleozoic', 'carboniferous', 'pennsylvanian', 'moscovian'    , 315.2   ,
    'paleozoic', 'carboniferous', 'pennsylvanian', 'bashkirian'   , 323.2   ,
    'paleozoic', 'carboniferous', 'mississippian', 'serpukhovian' , 330.9   ,
    'paleozoic', 'carboniferous', 'mississippian', 'viséan'       , 346.7   ,
    'paleozoic', 'carboniferous', 'mississippian', 'tournaisian'  , 358.9   ,
    'paleozoic', 'devonian'     , 'late'         , 'Famennian'    , 371.1   ,
    'paleozoic', 'devonian'     , 'late'         , 'Frasnian'     , 382.7   ,
    'paleozoic', 'devonian'     , 'middle'       , 'Givetian'     , 387.7   ,
    'paleozoic', 'devonian'     , 'middle'       , 'Eifelian'     , 393.3   ,
    'paleozoic', 'devonian'     , 'early'        , 'Emsian'       , 407.6   ,
    'paleozoic', 'devonian'     , 'early'        , 'Pragian'      , 410.8   ,
    'paleozoic', 'devonian'     , 'early'        , 'Lochkovian'   , 419.2   ,
    'paleozoic', 'silurian'     , 'pridoli'      , NA_character_  , 423.0   ,
    'paleozoic', 'silurian'     , 'ludlow'       , 'ludfordian'   , 425.6   ,
    'paleozoic', 'silurian'     , 'ludlow'       , 'gorstian'     , 427.4   ,
    'paleozoic', 'silurian'     , 'wenlock'      , 'homerian'     , 430.5   ,
    'paleozoic', 'silurian'     , 'wenlock'      , 'sheinwoodian' , 433.4   ,
    'paleozoic', 'silurian'     , 'llandovery'   , 'telychian'    , 438.5   ,
    'paleozoic', 'silurian'     , 'llandovery'   , 'aeronian'     , 440.8   ,
    'paleozoic', 'silurian'     , 'llandovery'   , 'rhuddanian'   , 443.8   ,
    'paleozoic', 'ordovician'   , NA_character_  , NA_character_  , 486.85  ,
  ) %>%
    dplyr::mutate(from = -abs(from)) %>%
    dplyr::mutate(to = from %>% lag() %>% replace_na(0)) %>%
    dplyr::mutate(across(c(era, period, epoch, age), str_to_sentence)) %>%
    truncate_geotime_labels()

  epoch_fill = tribble(
    ~period        , ~epoch         , ~fill   ,
    'quaternary'   , 'holocene'     , 'deeppink4',
    'quaternary'   , 'pleistocene'  , 'deeppink1',
    'neogene'      , 'pliocene'     , 'darkorchid4',
    'neogene'      , 'miocene'      , 'darkorchid1',
    'paleogene'    , 'oligocene'    , 'darkorange4',
    'paleogene'    , 'eocene'       , 'darkorange2',
    'paleogene'    , 'paleocene'    , 'darkorange1',
    'cretaceous'   , 'late'         , 'gold3',
    'cretaceous'   , 'early'        , 'gold1',
    'jurassic'     , 'late'         , 'steelblue4',
    'jurassic'     , 'middle'       , 'steelblue3',
    'jurassic'     , 'early'        , 'steelblue1',
    'triassic'     , 'late'         , 'tomato4',
    'triassic'     , 'middle'       , 'tomato2',
    'triassic'     , 'early'        , 'tomato1',
    'permian'      , 'lopingian'    , 'darkorchid4',
    'permian'      , 'guadalupian'  , 'darkorchid2',
    'permian'      , 'cisuralian'   , 'darkorchid1',
    'carboniferous', 'pennsylvanian', 'palegreen4',
    'carboniferous', 'mississippian', 'palegreen1',
    'devonian'     , 'late'         , 'dodgerblue4',
    'devonian'     , 'middle'       , 'dodgerblue2',
    'devonian'     , 'early'        , 'dodgerblue1',
    'silurian'     , 'pridoli'      , 'wheat4',
    'silurian'     , 'ludlow'       , 'wheat3',
    'silurian'     , 'wenlock'      , 'wheat2',
    'silurian'     , 'llandovery'   , 'wheat1',
    'ordovician'   , 'late'         , 'dodgerblue4',
    'ordovician'   , 'middle'       , 'dodgerblue2',
    'ordovician'   , 'early'        , 'dodgerblue1',
    'cambrian'     , 'furongian'    , 'tan4',
    'cambrian'     , 'miaolingian'  , 'tan3',
    'cambrian'     , 'series 2'     , 'tan2',
    'cambrian'     , 'terreneuvian' , 'tan1',
  ) %>%
    dplyr::mutate(across(c(period, epoch), str_to_sentence)) %>%
    truncate_geotime_labels()

  # scales::show_col(as.vector())
  #
  # x = 'blue'
  # scales::show_col(as.vector(x))
  #
  # y = shades::brightness(x, 0.5)
  # scales::show_col(as.vector(y))
  #
  # z = shades::saturation(y, c(0.1, 0.5, 1, 100))
  # scales::show_col(as.vector(z))
  #
  # x = shades::complement(c('red', 'blue', 'green'))
  # scales::show_col(as.vector(x))
  #
  # x = shades::complement(c('cyan', 'yellow', 'magenta'))
  # scales::show_col(as.vector(x))

  period_fill = epoch_fill %>%
    group_by(period) %>%
    summarise(fill = sort(fill)[1])

  # timescales = c('era', 'period', 'epoch', 'age') %>%
  #   keep(. %in% colnames(geotime))

  timescales = c('period', 'epoch', 'age')

  geotime = timescales %>%
    set_names(str_c(., 's')) %>%
    purrr::map(function(timescale){
      geotime %>%
        group_by(across(all_of(timescales[1:which(timescales == timescale)]))) %>%
        summarise(
          from=min(from),
          to=max(to),
          .groups = 'drop'
        ) %>%
        dplyr::arrange(desc(from))
    })

  geotime$periods %<>%
    left_join(period_fill, by='period')

  geotime$epochs %<>%
    left_join(epoch_fill, by=c('period', 'epoch'))

  geotime$ages %<>%
    left_join(epoch_fill, by=c('period', 'epoch'))

  # replace with short names
  # geotime$periods$period[tolower(geotime$periods$period == 'quaternary'] = 'Q.'
  # geotime$epochs$period[tolower(geotime$epochs$period == 'quaternary'] = 'Q.'

  # geotime$epochs$epoch[geotime$epochs$epoch == 'Paleocene'] = 'Paleoc.'
  # geotime$epochs$epoch[geotime$epochs$epoch == 'Eocene'] = 'Eoc.'
  # geotime$epochs$epoch[geotime$epochs$epoch == 'Oligocene'] = 'Oligoc.'
  # geotime$epochs$epoch[geotime$epochs$epoch == 'Miocene'] = 'Mioc.'
  # geotime$epochs$epoch[tolower(geotime$epochs$epoch) == 'pliocene'] = 'Pl.'
  # geotime$epochs$epoch[tolower(geotime$epochs$epoch) == 'pleistocene'] = 'Pl.'
  # geotime$epochs$epoch[tolower(geotime$epochs$epoch) == 'holocene'] = ''

  # geotime$periods$period %<>%
  #   str_replace('Quaternary', 'Q')
  #
  # geotime$epochs$period %<>%
  #   str_replace('Quaternary', 'Q')
  #
  # geotime$ages$period %<>%
  #   str_replace('Quaternary', 'Q')
  #
  # geotime$epochs$epoch %<>%
  #   str_replace('Pliocene', 'Pl.') %>%
  #   str_replace('Pleistocene', 'Pl.') %>%
  #   str_replace('Holocene', '') %>%
  #   str_replace('ocene$', 'oc.') %>%
  #   str_replace('ian$', '.')
  #
  # geotime$ages$age %<>%
  #   str_replace('ian$', '.')

  return(geotime)
}

truncate_geotime_labels = function(df) {
  if (!is.null(df[['period']])) {
    df$period %<>%
      str_replace('Quaternary', 'Q')
  }
  if (!is.null(df[['epoch']])) {
    df$epoch %<>%
      str_replace('Pliocene', 'Pl.') %>%
      str_replace('Pleistocene', 'Pl.') %>%
      str_replace('Holocene', '') %>%
      str_replace('ocene$', 'oc.') %>%
      str_replace('ian$', '.')
  }
  if (!is.null(df[['age']])) {
    df$age %<>%
      str_replace('ian$', '.')
  }
  return(df)
}

load_extinctions = function() {
  tribble(
    ~label, ~ma,
    'K-Pg', 66,
    'Tr-J', 201.3,
    'P-Tr', 251.902,
    'Late D', 372,
    'Late O', 445,
  ) %>%
    dplyr::mutate(ma = -abs(ma))
}

load_events = function() {
  tribble(
    ~label, ~ma,
    'GABI', 2.7,
  ) %>%
    dplyr::mutate(ma = -abs(ma))
}

check_tree_data = function(df) {

  # check names unique
  errors = df$name %>% table
  errors = errors[errors > 1]
  if (length(errors) > 0) {
    stop('non-unique names: ', names(errors))
  }

  # check each child has only parent
  children_counts = df$children %>% unlist %>% table
  errors = children_counts[children_counts > 1]
  if (length(errors) > 0) {
    stop('children with multiple parents: ', names(errors))
  }

  # check tips are not parents
  errors = df$name[df$is_tip][df$name[df$is_tip] %in% df$name[!df$is_tip]]
  if (length(errors) > 0) {
    stop('some tips are also parents: ', names(errors))
  }

  # check parents are not tips
  errors = df$name[!df$is_tip][df$name[!df$is_tip] %in% df$name[df$is_tip]]
  if (length(errors) > 0) {
    stop('some parents are also tips: ', names(errors))
  }

  # check children
  for (i in 1:nrow(df)) { #i=1
    if (!is.na(df$index[i])) next
    children = df$children[[i]]
    errors = children[!children %in% df$name]
    if(length(errors) > 0) {
      stop(df$name[i], ' children not in tips or nodes: ', str_c(errors, collapse = ', '))
    }
  }

  # check from
  for (i in 1:nrow(df)) { #i=1
    if (!is.na(df$index[i])) next
    children = df$children[[i]]
    child_from = df$from[df$name %in% children]
    if(!all(child_from >= df$from[i])) {
      stop('some children of ', df$name[i], ' have invalid from dates')
    }
  }

  # check parents
  roots = df$name[!df$name %in% unlist(df$children)]
  # if (length(roots) == 1) {
  #   message('root: ', roots)
  # }
  if (length(roots) > 1) {
    warning('multiple roots:\n', str_c(sort(roots), collapse = '\n'))
  }

  invisible(df)

}

reindex_tree_data = function(df) {

  tips = df %>%
    filter(is_tip) %>%
    # dplyr::arrange(desc(index)) %>%
    dplyr::mutate(index = 1:nrow(.)) %>%
    identity
  nodes = df %>%
    filter(!is_tip) %>%
    dplyr::mutate(index = NA_integer_)
  df = dplyr::bind_rows(tips, nodes) %>%
    dplyr::select(index, everything())

  while (any(is.na(df$index))) {
    for (i in 1:nrow(df)) { #i=1
      if (!is.na(df$index[i])) next
      children = df$children[[i]]
      child_indices = df$index[df$name %in% children]
      if (any(is.na(child_indices))) next
      df$index[i] = mean(child_indices)
      # df$index[i] = min(child_indices)
      # df$index[i] = max(child_indices)
    }
  }

  df %<>% dplyr::arrange(desc(is_tip), desc(index))

  return(df)

}

get_clade = function(df, clade) {
  if (is.null(clade)) return(df)
  names = c()
  temp_names = c(clade)
  while (length(temp_names) > 0) {
    names = c(names, temp_names)
    temp_df = df %>% filter(name %in% !!temp_names)
    temp_names = unlist(temp_df$children)
  }
  df_clade = df %>% filter(name %in% !!names)
  return(df_clade)
}

collapse_clade = function(df, clade) {
  if (is.null(clade)) return(df)
  if (!clade %in% df$name) {
    warning("can't collapse ", clade, ": does not exist")
    return(df)
  }
  df_clade = df %>% get_clade(clade)
  names_to_drop = df_clade$name[df_clade$name != clade]
  df = df[!df$name %in% names_to_drop, ]
  df$to[df$name == clade] = max(df_clade$to, na.rm=TRUE)
  df$is_tip[df$name == clade] = TRUE
  df$index[df$name == clade] = round(mean(df_clade$index, na.rm = TRUE))
  df$children[df$name == clade] = list(NULL)
  df = dplyr::bind_rows(
    df %>% filter(is_tip) %>% dplyr::arrange(desc(index)),
    df %>% filter(!is_tip)
  )
  return(df)
}

collapse_clades = function(df, clades=NULL) {
  if (is.null(clades)) return(df)
  for (clade in clades) {
    df = collapse_clade(df, clade)
  }
  return(df)
}

collapse_families = function(df) {
  families = df$name %>% keep(stringr::str_detect, 'idae$')
  for (family in families) {
    df = collapse_clade(df, clade=family)
  }
  return(df)
}

get_roots = function(df) {
  roots = df$name %>% discard(. %in% unlist(df$children))
  df %>% filter(name %in% roots)
}

# parent="pseudosuchia"
get_children = function(df, parent) {
  children = df$children[[which(df$name == parent)]]
  df %>% filter(name %in% !!children)
}
#
# # df=plt$data ; i=1 ; j=1 ; k=1
# order_tree_data = function(df) {
#   names = c()
#   gen0 = get_roots(df) %>% dplyr::arrange(from)
#   for (i in 1:nrow(gen0)) {
#     name = gen0$name[i]
#     names = c(names, name)
#     gen1 = get_children(df, name)
#     for (j in 1:nrow(gen1)) {
#       name = gen1$name[j]
#       names = c(names, name)
#       gen2 = get_children(df, name)
#       if
#       for (k in 1:nrow(gen2)) {
#         name = gen2$name[k]
#         names = c(names, name)
#         gen3 = get_children(df, name)
#       }
#     }
#   }
# }
#
# for
#
# get_children2 = function(name) {
#   if (df$is_tip[df$name == name]) {
#
#   } else {
#
#     get_children
#
#   }
#
# }

highlight_clade = function(plt, clade, col=1, alpha=0.25) {
  temp = plt$data %>% get_clade(clade)
  plt +
    annotate(
      "rect",
      xmin=min(temp$from) - 1,
      xmax=max(temp$to, na.rm=TRUE) + 1,
      ymin=min(temp$index) - 0.25,
      ymax=max(temp$index) + 0.5,
      alpha=alpha,
      fill=col,
      col=NA,
    )
}

rescale = function(x, new_min, new_max) {
  y = (x - min(x)) / diff(range(x))
  y = y * (new_max - new_min)
  y = y + new_min
  return(y)
}

sigmoid = function(x1, x2, y1, y2, l=5, n=20) {
  ys = plogis(seq(-l, l, length.out=n))
  ys = ys[ys > 0 & ys < 1]
  xs = qlogis(ys)
  xs = rescale(xs, x1, x2)
  ys = rescale(ys, y1, y2)
  return(tibble(x=xs, y=ys))
}

get_roots = function(df){
  i = !df$name %in% unlist(df$children)
  return (df[i, ])
}


get_parent = function(df, clade) {
  i = df$children %>% map_lgl(function(x) clade %in% x)
  stopifnot(sum(i) <= 1)
  return (df[i, ])
}


get_chlidren = function(df, clade) {
  children =  df$children[df$name == clade][[1]]
  i = df$name %in% children
  return (df[i, ])
}

is_root = function(df, clade) {
  nrow(get_parent(df, clade)) == 0
}

get_siblings = function(df, clade) {
  parent = get_parent(df, clade)
  if (nrow(parent) == 0) {
    siblings = get_roots(df)
  } else {
    siblings = get_chlidren(df, parent$name)
  }
  return (siblings)
}

add_depths = function(df) {
  depth = 1
  df$depth = NA_integer_
  children = get_roots(df)$name
  while (length(children) > 0) {
    df$depth[df$name %in% children] = depth
    children = unlist(df$children[df$name %in% children])
    depth = depth + 1
  }
  return(df)
}

# clade = 'theropoda'
# clade = 'dromaeosauridae'
# get_parent(df, clade)
# get_chlidren(df, clade)
# get_siblings(df, clade)

arrange_tree = function(df) { # df = plt$data ; print(df, n=Inf)
  max_depth = max(df$depth)
  rank_names = str_c('rank', 1:max_depth)
  df$ranks = purrr::map(1:nrow(df), function(x) NULL)
  for (i in 1:nrow(df)) { # i=44
    ranks = NULL
    clade = df$name[i]
    while (length(clade) > 0) {
      siblings = get_siblings(df, clade)
      rank = rank(siblings$from)[siblings$name == clade]
      ranks = c(rank, ranks)
      clade = get_parent(df, clade)$name
    }
    zeros = rep(0, max_depth - length(ranks))
    ranks = c(ranks, zeros) %>% set_names(rank_names)
    ranks
    df$ranks[[i]] = ranks
  }
  # df$ranks %>% set_names(df$name) %>% dplyr::bind_rows(.id = 'name') %>% print(n=Inf) %>% dplyr::arrange(across(one_of(rank_names)))
  df %>%
    dplyr::mutate(ranks = ranks %>% purrr::map(function(x) as_tibble(as.list(x)))) %>%
    unnest(ranks) %>%
    dplyr::arrange(across(one_of(rank_names))) %>%
    dplyr::select(-one_of(rank_names)) %>%
    reindex_tree_data() %>%
    # filter(name %in% c('tetanurae', 'ceratosauria')) %>%
    # print(n=Inf)
    identity
}

# plot(sigmoid(x1=0, x2=1, y1=0, y2=1), type='l')
# plot(sigmoid(x1=-10, x2=20, y1=0, y2=1), type='l')
# plot(sigmoid(x1=0, x2=1, y1=10, y2=5), type='l')

plt = tree(collapse=c('amniota')) ; print(plt)
stop('deliberate')

plt = tree('amniota', collapse=c('archosauromorpha', 'therapsida', 'lepidosauromorpha')) ; print(plt)

plt = tree('therapsida', collapse='mammalia') ; print(plt)

plt = tree('mammalia') ; print(plt)

plt = tree('archosauromorpha', collapse=c('dinosauria', 'pterosauria', 'pseudosuchia', 'sauropterygia')) ; print(plt)

plt = tree('pseudosuchia') ; print(plt)

plt = tree('dinosauria', collapse = c('sauropoda', 'ornithopoda', 'theropoda', 'ankylosauria', 'ceratopsia')) ; print(plt)

plt = tree(clade='theropoda', collapse=c('avialae')) ; print(plt)

plt = tree(collapse = c('pseudosuchia',
                        'dinosauria',
                        'pterosauria',
                        'synapsida',
                        'lepidosauromorpha',
                        'temnospondyli',
                        'sauropterygia',
                        'parareptilia')) ; print(plt)

plt = tree('dinosauria', collapse = c('sauropodomorpha', 'theropoda')) ; print(plt)

plt = tree('dinosauria', collapse = c('ornithischia', 'theropoda')) ; print(plt)

plt = tree('dinosauria', collapse = c('sauropodomorpha', 'ornithischia', 'avialae')) ; print(plt)

plt = tree(
  clade='dinosauria',
  highlight = list(
    theropoda='red',
    sauropodomorpha='green',
    ornithischia='blue'
  ),
  collapse = c(
    'avialae'
  )
) ; print(plt)

# plt = tree(clade='dinosauromorpha', highlight = list(ornithischia='grey25'))
# print(plt) ; stop('deliberate')

# plt = tree(clade='sauropterygia') ; print(plt)
# stop('deliberate')
#
# plt = tree(collapse='amniota') ; print(plt)
# stop('deliberate')

# plt = tree(clade='synapsida') ; print(plt)
# stop('deliberate')

plt = tree(clade='paraves') ; print(plt)

plt = tree(clade='avialae') ; print(plt)

plt = tree(clade='sauropodomorpha') ; print(plt)

plt = tree(clade='ornithischia') ; print(plt)

# plt = tree(clade='pterosauria')
# print(plt) ; stop('deliberate')

# plt = tree(clade='pseudosuchia')
# print(plt) ; stop('deliberate')

# plt = tree(clade='theropoda') %>% highlight_clade('dromaeosauridae', 'grey25')
# print(plt) ; stop('deliberate')

# plt = tree(collapse=list('amniota'))
# print(plt) ; stop('deliberate')

# plt = tree()
# print(plt) ; stop('deliberate')



df = plt$data
df %>% print(n=Inf)
df %>% print.data.frame()




