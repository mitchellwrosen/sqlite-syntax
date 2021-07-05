Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "t1"
                }
            , alias = Nothing
            }
        , columns = Just
            ( "id" :| [ "name" ] )
        , insert = InsertSelect
            ( SelectStatement
                { commonTableExpressions = Nothing
                , select = CompoundSelect
                    ( SelectCore'Values
                        ( Values
                            (
                                ( Expression'LiteralValue
                                    ( Number "1" ) :|
                                    [ Expression'LiteralValue
                                        ( String "A1234567890B1234567890C1234567890D1234567890E1234567890F1234567890G1234567890H1234567890I1234567890J1234567890K1234567890L1234567890M1234567890N1234567890O1234567890P1234567890Q1234567890R1234567890" )
                                    ]
                                ) :| []
                            )
                        )
                    )
                , orderBy = Nothing
                , limit = Nothing
                }
            ) Nothing
        , returning = Nothing
        }
    )