Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "tx"
                }
            , alias = Nothing
            }
        , columns = Nothing
        , insert = InsertDefaultValues
        , returning = Nothing
        }
    )