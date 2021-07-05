Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'LiteralValue
                                ( Number "99" )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Nothing
                    , where_ = Just
                        ( Expression'LiteralValue
                            ( Number "0.0" )
                        )
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )