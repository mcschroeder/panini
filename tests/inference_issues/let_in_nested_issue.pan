nestedLet : { i : int| true } -> {l: int | l = i}
nestedLet = \(i:int).
        let id = \(x:int).x
        in
        let r = id i in
        r
