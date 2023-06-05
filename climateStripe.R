

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
                             x_breaks=seq.Date(as.Date("1972-01-01"),
                                               as.Date("1972-12-01"),
                                               "months"),
                             y_breaks=NULL,
                             x_label_format="%m",
                             y_label_format="%Y",
                             x_date_breaks=ggplot2::waiver(),
                             y_date_breaks="years",
                             x_expand=ggplot2::expansion(add=c(0, 1)),
                             y_expand=ggplot2::expansion(add=c(1, 0)),
                             y_position="right",
                             axis_margin=ggplot2::margin(0, 0, 0, 0,
                                                         unit="mm")) {
    
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
    stripe_max_xmax = stripe_max_xmin+1

    climateStripe = ggplot2::ggplot() + ggplot2::theme_void() +
        ggplot2::theme(plot.margin=axis_margin)

    if (is_x_axis) {
        climateStripe =
            climateStripe +
            ggplot2::theme(axis.text.x=
                               ggplot2::element_text(color=IPCCgrey40,
                                                     size=x_size,
                                                     hjust=0))
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
                               ggplot2::element_text(color=IPCCgrey40,
                                                     size=y_size,
                                                     vjust=-0.1))
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


Date = seq.Date(as.Date("1970-01-01"), as.Date("2020-12-31"), "day")
Value = 1:length(Date)/1000 + rnorm(length(Date), 0, 2) - 10

plot = gg_climateStripe(Date, Value,
                        palette_name='BrBG',
                        palette_n_shade=8,
                        palette_reverse=TRUE,
                        palette_is_center=TRUE,
                        palette_q_extrem=0,
                        stripe_space_factor=0,
                        stripe_chunk_by="years",
                        stripe_chunk_reverse=TRUE,
                        is_x_axis=TRUE,
                        is_y_axis=TRUE,
                        x_label_format="%b",
                        y_label_format="%Y",
                        x_breaks=seq.Date(as.Date("1972-01-01"),
                                          as.Date("1972-12-01"),
                                          "months"),
                        x_date_breaks=ggplot2::waiver(),
                        y_date_breaks="years",
                        x_expand=ggplot2::expansion(add=c(0, 2)),
                        y_expand=ggplot2::expansion(add=c(10, 0)),
                        y_position="right",
                        axis_margin=ggplot2::margin(1, 1, 1, 1,
                                                    unit="mm"))

ggplot2::ggsave(plot=plot,
                filename="climateStripe.pdf",
                width=2000, height=6000, units="px")
