;;; Open Workstation for head or side

function openWksHd(plot_param)
begin
  wks = create (OutDir + "../../" + plot_param + ".head") imageWorkstationClass defaultapp
                "wkHeight"          : wkHeight
                "wkWidth"           : wkWidth
                "wkBackgroundColor" : "white"
                "wkForegroundColor" : "black"
                "wkColorMap"        : "BlAqGrYeOrReVi200"      ; probably best by DrJack
		end create
  return(wks)
end

function openWksLb(plot_param)
begin
    wks = create (OutDir + "../../" + plot_param + ".foot") imageWorkstationClass defaultapp
			"wkHeight"          : wkHeight
			"wkWidth"           : wkWidth
			"wkBackgroundColor" : "white"
			"wkForegroundColor" : "black"
			"wkColorMap"        : "BlAqGrYeOrReVi200"      ; probably best by DrJack
		end create
    return(wks)
end

