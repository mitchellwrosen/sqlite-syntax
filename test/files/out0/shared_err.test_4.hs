Statement'Delete
    ( DeleteStatement
        { commonTableExpressions = Nothing
        , table = QualifiedTableName
            { name = Aliased
                { value = Namespaced
                    { namespace = Nothing
                    , value = "t1"
                    }
                , alias = Nothing
                }
            , indexedBy = Nothing
            }
        , where_ = Just
            ( Expression'Equals
                ( Expression'LiteralValue
                    ( Number "0" )
                )
                ( Expression'Modulo
                    ( Expression'Column
                        ( Namespaced
                            { namespace = Nothing
                            , value = "a"
                            }
                        )
                    )
                    ( Expression'LiteralValue
                        ( Number "2" )
                    )
                )
            )
        , returning = Nothing
        }
    )