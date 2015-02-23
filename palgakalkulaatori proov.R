#NETOpalk
tm.vaba=154
tm.maar=0.2
sm.maar=0.33
kp.maar=0.02
tk.maar.tootaja=0.016
tk.maar.tooandja=0.008
palk.bruto=1000

#arvutus
sm=palk.bruto*sm.maar
tk.tootaja=palk.bruto*tk.maar.tootaja
tk.tooandja=palk.bruto*tk.maar.tooandja
kp=palk.bruto*kp.maar
tm=(palk.bruto-tm.vaba-tk.tootaja-kp)*tm.maar

palk.neto=palk.bruto-tk.tootaja-kp-tm

tooandja.kulu=palk.bruto+sm+tk.tooandja
maksuametile.koik=sm+tk.tootaja+tk.tooandja+tm+kp
tulem=c(palk.neto, tooandja.kulu, maksuametile.koik, tm, sm,kp,
        tk.tootaja, tk.tooandja)
tulem.names=c("Netopalk", "Tööandja kulu", "Maksuametile kantav summa", "Tulumaks",
              "Sotsiaalmaks", "Kogumispension", "Töötuskindlustus (töötaja)",
              "Töötuskindlustus(tööandja)")
tulem.kokku=data.frame(tulem.names, tulem)
names(tulem.kokku)=c(" ", "Summa (€)")

netopalk <- function(palk.bruto, tm.vaba=154, tm.maar=0.2, sm.maar=0.33, 
                      kp.maar=0.02, tk.maar.tootaja=0.016, tk.maar.tooandja=0.008){
    #arvutused
    sm=round(palk.bruto*sm.maar,2)
    tk.tootaja=round(palk.bruto*tk.maar.tootaja,2)
    tk.tooandja=round(palk.bruto*tk.maar.tooandja, 2)
    kp=round(palk.bruto*kp.maar, 2)
    tm=round((palk.bruto-tm.vaba-tk.tootaja-kp)*tm.maar,2)
    if(tm<=0) {
        tm=0.00
        round(tm,2)
    }
    #palk
    palk.neto=round(palk.bruto-tk.tootaja-kp-tm,2)
    #kulu eri osapooltele
    tooandja.kulu=round(palk.bruto+sm+tk.tooandja, 2)
    maksuametile.koik= round(sm+tk.tootaja+tk.tooandja+tm+kp, 2)
    #tulemite koondamine
    tulem=c(palk.bruto,palk.neto, tooandja.kulu, maksuametile.koik, tm, sm,kp,
            tk.tootaja, tk.tooandja)
    tulem.names=c("Brutopalk", "Netopalk", "Tööandja kulu", 
                  "Maksuametile kantav summa", "Tulumaks","Sotsiaalmaks", 
                  "Kogumispension", "Töötuskindlustus (töötaja)",
                  "Töötuskindlustus(tööandja)")
    tulem.kokku=data.frame(tulem.names, tulem)
    names(tulem.kokku)=c(" ", "Summa (€)")
    #tulemi esitamine
    print(tulem.kokku, row.names=F)
}

netopalk(1000)
netopalk(154)
netopalk(90)

##BRUTOpalk
#kalkulaator brutopalga leidmiseks
tm.vaba=154
tm.maar=0.2
sm.maar=0.33
kp.maar=0.02
tk.maar.tootaja=0.016
tk.maar.tooandja=0.008
palk.neto=1000

#arvutus
palk.bruto=round((palk.neto-tm.vaba*tm.maar)/
                     (1-tk.maar.tootaja-kp.maar-tm.maar+
                          tk.maar.tootaja*tm.maar+kp.maar*tm.maar),2)
sm=round(palk.bruto*sm.maar, 2)
tk.tootaja=round(palk.bruto*tk.maar.tootaja, 2)
tk.tooandja=round(palk.bruto*tk.maar.tooandja, 2)
kp=round(palk.bruto*kp.maar, 2)
tm=round((palk.bruto-tm.vaba-tk.tootaja-kp)*tm.maar, 2)

tooandja.kulu=palk.bruto+sm+tk.tooandja
maksuametile.koik=sm+tk.tootaja+tk.tooandja+tm+kp
tulem=c(palk.bruto, palk.neto, tooandja.kulu, maksuametile.koik, tm, sm,kp,
        tk.tootaja, tk.tooandja)
tulem.names=c("Brutopalk", "Netopalk", "Tööandja kulu", "Maksuametile kantav summa",
              "Tulumaks","Sotsiaalmaks", "Kogumispension", 
              "Töötuskindlustus (töötaja)","Töötuskindlustus(tööandja)")
tulem.kokku=data.frame(tulem.names, tulem)
names(tulem.kokku)=c(" ", "Summa (€)")


#funktsioon
brutopalk <- function(palk.neto, tm.vaba=154, tm.maar=0.2, sm.maar=0.33, 
                     kp.maar=0.02, tk.maar.tootaja=0.016, tk.maar.tooandja=0.008){
    #arvutused
    if(palk.neto<=tm.vaba){
        tm.maar=0
    }
    palk.bruto=round((palk.neto-tm.vaba*tm.maar)/
                         (1-tk.maar.tootaja-kp.maar-tm.maar+
                              tk.maar.tootaja*tm.maar+kp.maar*tm.maar),2)
    sm=round(palk.bruto*sm.maar, 2)
    tk.tootaja=round(palk.bruto*tk.maar.tootaja, 2)
    tk.tooandja=round(palk.bruto*tk.maar.tooandja, 2)
    kp=round(palk.bruto*kp.maar, 2)
    tm=round((palk.bruto-tm.vaba-tk.tootaja-kp)*tm.maar, 2)
    #kulu eri osapooltele
    tooandja.kulu=palk.neto+sm+tk.tooandja+tm+kp+tk.tootaja
    maksuametile.koik=sm+tk.tootaja+tk.tooandja+tm+kp
    #arvutame uuesti brutopalga, kuna eelmine on liiga täpne ning tagurpidi 
    #arvutades tekib komakohtades erinevus
    palk.bruto=palk.neto+tm+kp+tk.tootaja
    #tulemite koondamine
    tulem=c(palk.bruto, palk.neto, tooandja.kulu, maksuametile.koik, tm, sm,kp,
            tk.tootaja, tk.tooandja)
    tulem.names=c("Brutopalk", "Netopalk", "Tööandja kulu", "Maksuametile kantav summa", "Tulumaks",
                  "Sotsiaalmaks", "Kogumispension", "Töötuskindlustus (töötaja)",
                  "Töötuskindlustus (tööandja)")
    tulem.kokku=data.frame(tulem.names, tulem)
    names(tulem.kokku)=c(" ", "Summa (€)")
    #tulemi esitamine
    print(tulem.kokku, row.names=F)
}

brutopalk(1000)
#proovime ka nullmääradega
brutopalk(1000, tm.vaba=0, tm.maar=0.2, sm.maar=0.33, 
kp.maar=0, tk.maar.tootaja=0.016, tk.maar.tooandja=0.008)


plot(brutopalk(1000))

brutopalk(154)






