module Fall2014

    module Fall2014Q2 = 
        let multTable n = seq{ for i in 1 .. 10 -> i*n }

        //let tableOf m n f = seq{}

        let charRepeat c = Seq.toList (Seq.initInfinite (fun i-> String.replicate i c))
