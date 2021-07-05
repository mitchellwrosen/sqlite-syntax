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
            ( Expression'And
                ( Expression'GreaterThanOrEquals
                    ( Expression'Column
                        ( Namespaced
                            { namespace = Nothing
                            , value = "id"
                            }
                        )
                    )
                    ( Expression'LiteralValue
                        ( Number "1" )
                    )
                )
                ( Expression'LessThanOrEquals
                    ( Expression'Column
                        ( Namespaced
                            { namespace = Nothing
                            , value = "id"
                            }
                        )
                    )
                    ( Expression'LiteralValue
                        ( Number "25" )
                    )
                )
            )
        , returning = Nothing
        }
    )