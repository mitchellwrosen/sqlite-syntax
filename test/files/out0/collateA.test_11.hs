Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Equals
                                ( Expression'LiteralValue
                                    ( String "abc123" )
                                )
                                ( Expression'Collate
                                    ( CollateExpression
                                        { expression = Expression'LiteralValue
                                            ( String "abc123                         " )
                                        , collation = "RTRIM"
                                        }
                                    )
                                )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Nothing
                    , where_ = Nothing
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )