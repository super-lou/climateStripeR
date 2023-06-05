

IPCCgrey99 = "#f8f9f9" # lighter lighter plot background
IPCCgrey97b = "#f9f8f7" # lighter hot plot background
IPCCgrey97 = "#f6f7f7" # lighter blue plot background
IPCCgrey95 = "#f4f2f1" # plot background
IPCCgrey92 = "#e9eceb" # 
IPCCgrey90 = "#e3e2e0" # minor tick
IPCCgrey85 = "#dcdad9" # grid on grey background low important axis 
IPCCgrey80 = "#cfd1d0" # grid on white background
IPCCgrey75 = "#bebdbb" # major tick
IPCCgrey67 = "#adabaa" # minor line
IPCCgrey60 = "#9c9c9b" # important axis
IPCCgrey50 = "#81848b" # low important annotation
IPCCgrey48 = "#847b73" # major line
IPCCgrey40 = "#656769" # low important label
IPCCgrey25 = "#454547"
IPCCgrey20 = "#060403" # important title, label or annotation
IPCCgrey18 = "#2f2f32" # low important title
IPCCgrey13 = "#231f20" # font
IPCCgrey05 = "#100f0d" # realy important title
IPCCcyan = "#449c93"
IPCCligthcyan = "#90d6c6"
IPCCwhitecyan = "#a8ded3"
IPCCbrique = "#794822"
IPCCgold = "#e6d495"
IPCCblue = "#1e2f59"

INRAEcyan = "#00a3a6"
INRAElightcyan = "#66c1bf"
INRAEmediumcyan = "#008c8e"
INRAEdarkcyan = "#275662"
INRAElightblue = "#9ed6e3"


#' @title Compute color bin
#' @export
compute_colorBin = function (min, max, colorStep, center=TRUE,
                             include=FALSE) {

    if (center) {
        # Computes the absolute max
        maxAbs = max(abs(max), abs(min))
        minValue = -maxAbs
        maxValue = maxAbs
    } else {
        minValue = min
        maxValue = max
    }
    
    if (include) {
        nBin = colorStep + 1
    } else {
        nBin = colorStep - 1
    } 
    
    bin = seq(minValue, maxValue, length.out=nBin)
    if (!include) {
        upBin = c(bin, Inf)
        lowBin = c(-Inf, bin)
        bin = c(-Inf, bin, Inf)
    } else {
        upBin = bin[2:length(bin)]
        lowBin = bin[1:(length(bin)-1)]
    }

    midBin = zoo::rollmean(bin, 2)

    res = list(bin=bin, upBin=upBin,
               midBin=midBin, lowBin=lowBin)
    return (res)
}


get_color = function (value, upBin, lowBin, Palette) {
    id = which(value <= upBin & value > lowBin)
    color = Palette[id]
    return (color)
} 


#' @title Get palette
#' @export
get_palette = function (palette_name, colorStep=256,
                        reverse=FALSE) {

    if (length(palette_name) > 1) {
        Palette = palette_name
    } else {
        Palette = RColorBrewer::brewer.pal(n=colorStep,
                                           name=palette_name)
    }
    Palette = colorRampPalette(Palette)(colorStep)
    if (reverse) {
        Palette = rev(Palette)
    }
    return (Palette)
}


get_chunk = function(X, breaks) {
    if (!is.null(breaks)) {
        chunk = split(X, cut(X, breaks, labels=FALSE))
    } else {
        chunk = list(X)
        names(chunk) = 1
    }
    return (chunk)
}



get_breaks_function = function (breaks, isDate=TRUE,
                                d_breaks=0,
                                break_round=-1,
                                add_breaks=NULL,
                                rm_breaks=NULL) {

    get_breaks = function (X) {
        if (isDate) {
            Xmin = round(lubridate::year(min(X)), break_round)
            Xmax = round(lubridate::year(max(X)), break_round)
            if (Xmax-Xmin <= 1) {
                Xmin = lubridate::year(X)[1]
                Xmax = lubridate::year(X)[1] + 1
            }
            res = seq.Date(from=as.Date(paste0(Xmin, "-01-01")) +
                               d_breaks,
                           to=as.Date(paste0(Xmax, "-01-01")) +
                               d_breaks,
                           by=breaks)
        } else {
            Xmin = round(min(X), break_round)
            Xmax = round(max(X), break_round)
            res = seq(from=Xmin + d_breaks,
                      to=Xmax + d_breaks,
                      by=breaks)
        }

        if (!is.null(add_breaks)) {
            res = sort(c(res, add_breaks))
        }

        if (!is.null(rm_breaks)) {
            res = res[!(res %in% rm_breaks)]
        }

        return (res)
    }

    return (get_breaks)
}


nbsp = function (n) {
    paste0(rep("<span> </span>", times = n), collapse = "")
}

gg_climateStripe = function (Date, Value,
                             palette_name='RdBu',
                             palette_n_shade=8,
                             palette_reverse=FALSE,
                             palette_is_center=TRUE,
                             palette_q_extrem=0,
                             stripe_space_factor=0,
                             stripe_chunk_by=NULL,
                             stripe_chunk_reverse=FALSE,
                             is_x_axis=FALSE,
                             is_y_axis=FALSE,
                             x_size=NULL,
                             y_size=14,
                             x_color=NULL,
                             y_color=NULL,
                             x_hjust=NULL,
                             x_vjust=NULL,
                             y_hjust=NULL,
                             y_vjust=-0.1,
                             x_breaks=seq.Date(as.Date("1972-01-01"),
                                               as.Date("1972-12-01"),
                                               "months"),
                             y_breaks=NULL,
                             x_label_format="%m",
                             y_label_format="%Y",
                             x_date_breaks=NULL,
                             y_date_breaks="years",
                             x_expand=ggplot2::expansion(add=c(0, 1)),
                             y_expand=ggplot2::expansion(add=c(1, 0)),
                             y_position="right",
                             axis_margin=ggplot2::margin(0, 0, 0, 0,
                                                         unit="mm")) {

    if (is.null(x_date_breaks)) {
        x_date_breaks = ggplot2::waiver()
    }
    if (is.null(y_date_breaks)) {
        y_date_breaks = ggplot2::waiver()
    }
    
    chunkDate = get_chunk(Date, stripe_chunk_by)

    dDate = Date[2] - Date[1]
    i = 1
    while (is.na(dDate)) {
        i = i + 1
        dDate = Date[i+1] - Date[i]
    }

    nChunk = length(chunkDate)
    
    minValue_tmp = quantile(Value, palette_q_extrem)
    maxValue_tmp = quantile(Value, 1-palette_q_extrem)
    minValue = min(c(minValue_tmp, maxValue_tmp))
    maxValue = max(c(minValue_tmp, maxValue_tmp))

    if (palette_q_extrem %in% c(0, 1)) {
        include = TRUE
    } else {
        include = FALSE
    }

    colorStep = palette_n_shade*2+1
    Palette = get_palette(palette_name, colorStep, palette_reverse)
    res = compute_colorBin(minValue, maxValue,
                           colorStep,
                           center=palette_is_center,
                           include=include)

    stripe_color = unlist(sapply(Value, get_color,
                                 upBin=res$upBin,
                                 lowBin=res$lowBin,
                                 Palette=Palette))

    nStripeALL = sapply(chunkDate, length)
    nStripe_max = max(nStripeALL, na.rm=TRUE)
    
    stripe_space = dDate * stripe_space_factor

    stripe_max_xmin = chunkDate[nStripeALL == nStripe_max][[1]]
    stripe_max_xmax = stripe_max_xmin+dDate

    climateStripe = ggplot2::ggplot() + ggplot2::theme_void() +
        ggplot2::theme(plot.margin=axis_margin)

    if (is.null(x_color)) {
        x_color = IPCCgrey40
    }
    if (is.null(y_color)) {
        y_color = IPCCgrey40
    }
    
    if (is_x_axis) {
        climateStripe =
            climateStripe +
            ggplot2::theme(axis.text.x=
                               ggplot2::element_text(color=x_color,
                                                     size=x_size,
                                                     hjust=x_hjust,
                                                     vjust=x_vjust))
    } else {
        x_breaks = NULL
        x_date_breaks = ggplot2::waiver()
        x_label_format = ggplot2::waiver()
        x_expand = c(0, 0)
    }
    
    if (is_y_axis) {
        climateStripe =
            climateStripe +
            ggplot2::theme(axis.text.y=
                               ggplot2::element_text(color=y_color,
                                                     size=y_size,
                                                     hjust=y_hjust,
                                                     vjust=y_vjust))
    } else {
        y_breaks = NULL
        y_date_breaks = ggplot2::waiver()
        y_label_format = ggplot2::waiver()
        y_expand = c(0, 0)
    }


    for (i in 1:nChunk) {
        chunk = chunkDate[[i]]
        nStripe = length(chunk)

        stripe_xmin = stripe_max_xmin[1:nStripe]
        stripe_xmax = stripe_max_xmax[1:nStripe]
        
        stripe_ymin = rep(chunk[1], nStripe)
        stripe_ymax = rep(chunk[length(chunk)], nStripe)
        if (i < nChunk) {
            stripe_ymax = stripe_ymax + dDate - stripe_space
        }

        fill = stripe_color[match(chunk, Date)]

        climateStripe = climateStripe +
            ggplot2::annotate("rect",
                              xmin=stripe_xmin,
                              xmax=stripe_xmax,
                              ymin=stripe_ymin,
                              ymax=stripe_ymax,
                              fill=fill,
                              color=NA)
    }

    climateStripe = climateStripe +
        
        ggplot2::scale_x_date(breaks=x_breaks,
                              date_breaks=x_date_breaks,
                              date_labels=x_label_format,
                              expand=x_expand) +
        
        ggplot2::scale_y_date(breaks=y_breaks,
                              date_breaks=y_date_breaks,
                              date_labels=y_label_format,
                              expand=y_expand,
                              position=y_position)

    # if (stripe_chunk_reverse) {

    # library(scales)

    # reverse2_trans <- function() {
    #     trans_new(
    #         "reverse2",
    #         function(x) -1 * as.numeric(x), # Force values to be numeric for Date objects
    #         function(x) -1 * as.numeric(x)
    #     )
    # }

    # ggplot(economics, aes(date, unemploy)) +
    #     geom_line() +
    #     scale_x_continuous(
    #         trans = c("date", "reverse2")
    #     )
    
    # climateStripe = climateStripe + ggplot2::scale_y_reverse()
    # }
    
    return (climateStripe)
}


library(ggplot2)
library(gridExtra) # arrangeGrob

width = 1920
height_title = 50
height_stripe = 720
height = height_title + height_stripe

data_path = "/home/louis/Documents/bouleau/INRAE/project/Explore2_project/Explore2_toolbox/results/proj"

infoTraject =
    c("Fort réchauffement et fort assèchement en été",
      "Sec toute l’année, recharge moindre en hiver",
      "Scénario modéré en réchauffement et changement de précipitations",
      "Scénario chaud et humide à toutes les saisons")
nameTraject =
    c(
        "HadGEM2-ES_RCP_CCLM4-8-17_CDFt",
        "EC-EARTH_RCP_HadREM3-GA7_CDFt",
        "CNRM-CM5_RCP_ALADIN63_ADAMONT",
        "HadGEM2-ES_RCP_ALADIN63_ADAMONT"
    )

rcp = "rcp85"
nameTraject = gsub("RCP", rcp, nameTraject)

Var =
    c(
        "QA",
        "VCN10"
    )
river_selection =
    c(
        "Seine"="H700011001",
        "Rhone"="V720001002",
        "Garonne"="O972001000",
        "Loire"="M842001000",
        "Moselle"="A886006000"
    )

Model =
    c(
        "CTRIP",
        "EROS",
        "GRSD",
        "J2000",
        "SIM2",
        "MORDOR-SD",
        "MORDOR-TS",
        "ORCHIDEE",
        "SMASH"
    )


analyse = "dataEX_Explore2_proj_serie"




save_stripes = function (Date, Value,
                         traj, model, river, code, var,
                         width, height_title, height_stripe,
                         height) {
    
    plan = matrix(c("title", "stripe"), ncol=1)
    flock = dataSHEEP::bring_grass()
    flock = dataSHEEP::plan_of_flock(flock, plan)

    title = ggplot2::ggplot() + ggplot2::theme_void() +
        
        ggplot2::theme(plot.margin=
                           ggplot2::margin(1, 1, 0, 1,
                                           unit="mm"))

    label = paste0("<b style='font-size:10pt;
                                 color:", IPCCgrey25, "'>",
                   river, "</b> ", nbsp(1),
                   "<span style='font-size:6pt;
                                 color:", IPCCgrey60, "'>",
                   var, nbsp(2),
                   traj, "_",
                   model, "</span>", nbsp(2),
                   "<span style='font-size:6pt;
                                 color:", IPCCgrey60, "'>",
                   code,
                   "</span>")
    
    title = title +
        ggtext::geom_richtext(
                    aes(x=0,
                        y=1,
                        label=label),
                    hjust=0, vjust=1,
                    color=IPCCgrey25,
                    fill=NA, label.color=NA,
                    label.padding=grid::unit(rep(0, 4),
                                             "pt"))
    
    title = title +
        ggplot2::scale_x_continuous(limits=c(0, 1),
                                    expand=c(0, 0)) +
        ggplot2::scale_y_continuous(limits=c(0, 1),
                                    expand=c(0, 0))
    
    flock = dataSHEEP::add_sheep(flock,
                                 title,
                                 "title",
                                 height=height_title/height)
    
    stripe = gg_climateStripe(
        Date, Value,
        palette_name='BrBG',
        palette_reverse=FALSE,
        palette_is_center=FALSE,
        palette_q_extrem=0.01,
        is_x_axis=TRUE,
        x_size=8,
        x_vjust=0,
        x_color=IPCCgrey25,
        x_breaks=get_breaks_function(
            "10 years",
            rm_breaks=as.Date("2100-01-01")),
        x_label_format="%Y",
        x_date_breaks=NULL,
        axis_margin=ggplot2::margin(0.5, 1, 1, 1,
                                    unit="mm"))

    flock = dataSHEEP::add_sheep(flock,
                                 stripe,
                                 "stripe",
                                 height=height_stripe/height)
    
    res = dataSHEEP::return_to_sheepfold(
                         flock,
                         paper_size=c(width=width,
                                      height=height),
                         hjust=0, vjust=1,
                         verbose=TRUE)
    
    plot = res$plot
    paper_size = res$paper_size

    outdir_png = file.path("stripes", "png")
    if (!dir.exists(outdir_png)) {
        dir.create(outdir_png, recursive=TRUE)
    }

    outdir_pdf = file.path("stripes", "pdf")
    if (!dir.exists(outdir_pdf)) {
        dir.create(outdir_pdf, recursive=TRUE)
    }
    
    ggplot2::ggsave(plot=plot,
                    path=outdir_png, 
                    filename=paste0("stripes_",
                                    traj, "_",
                                    model, "_",
                                    river, "_",
                                    code, "_",
                                    var, 
                                    ".png"),
                    width=width, height=height, units="px")
    
    ggplot2::ggsave(plot=plot,
                    path=outdir_pdf, 
                    filename=paste0("stripes_",
                                    traj, "_",
                                    model, "_",
                                    river, "_",
                                    code, "_",
                                    var, 
                                    ".pdf"),
                    width=width, height=height, units="px")
}

# meta_path = file.path(data_path,
#                       model,
#                       traj_model,
#                       "meta.fst")
# if (!file.exists(meta_path)) {
#     next
# }

# meta = ASHE::read_tibble(meta_path)


# meta_selection = meta[grepl(river, meta$Nom),]
# code = meta_selection$Code[which.max(
#                           meta_selection[[paste0(
#                                              "Surface_",
#                                              model,
#                                              "_km2")]])]

# if (length(code) == 0) {
#     next
# }


nRiver = length(river_selection)

for (traj in nameTraject) {
    
    for (i in 1:nRiver) {
        river = names(river_selection)[i]
        code = river_selection[i]
        
        for (var in Var) {

            Value_multi = list()
            
            for (model in Model) {            
                
                traj_model = paste0(traj, "_", model)

                meta_path = file.path(data_path,
                                      model,
                                      traj_model,
                                      "meta.fst")
                if (!file.exists(meta_path)) {
                    next
                }
                
                data = ASHE::read_tibble(filepath=
                                             file.path(data_path,
                                                       model,
                                                       traj_model,
                                                       analyse,
                                                       "fst",
                                                       paste0(var,
                                                              ".fst")))

                data_code = data[data$Code == code,]
                if (nrow(data_code) == 0) {
                    next
                }
                
                Date = data_code$Date
                Value = data_code[[var]]
                Date = Date[!is.na(Value)]
                Value = Value[!is.na(Value)]

                Value_multi = append(Value_multi, list(Value))

                # save_stripes(Date, Value,
                #              traj, model, river, code, var,
                #              width, height_title, height_stripe,
                #              height)
            }

            Value_multi = matrix(unlist(Value_multi),
                                 nrow=length(Value_multi),
                                 byrow=TRUE)
            
            Value_med = apply(Value_multi, 2, mean)
            
            save_stripes(Date, Value_med,
                         traj, "MULTI", river, code, var,
                         width, height_title, height_stripe,
                         height)
        }
    }
}


# Date = seq.Date(as.Date("1970-01-01"), as.Date("2020-12-31"), "day")
# Value = 1:length(Date)/1000 + rnorm(length(Date), 0, 2) - 10

# plot = gg_climateStripe(Date, Value,
#                         palette_name='RdBu',
#                         palette_n_shade=8,
#                         palette_reverse=TRUE,
#                         palette_is_center=TRUE,
#                         palette_q_extrem=0,
#                         stripe_space_factor=0,
#                         stripe_chunk_by="years",
#                         stripe_chunk_reverse=TRUE,
#                         is_x_axis=TRUE,
#                         is_y_axis=TRUE,
#                         x_label_format="%b",
#                         y_label_format="%Y",
#                         x_breaks=seq.Date(as.Date("1972-01-01"),
#                                           as.Date("1972-12-01"),
#                                           "months"),
#                         x_date_breaks=ggplot2::waiver(),
#                         y_date_breaks="years",
#                         x_expand=ggplot2::expansion(add=c(0, 2)),
#                         y_expand=ggplot2::expansion(add=c(10, 0)),
#                         y_position="right",
#                         axis_margin=ggplot2::margin(1, 1, 1, 1,
#                                                     unit="mm"))

# ggplot2::ggsave(plot=plot,
# filename="climateStripe.pdf",
# width=2000, height=6000, units="px")
