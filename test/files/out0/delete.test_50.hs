Statement'Delete
    ( DeleteStatement
        { commonTableExpressions = Nothing
        , table = QualifiedTableName
            { name = Aliased
                { value = Namespaced
                    { namespace = Nothing
                    , value = "table2"
                    }
                , alias = Nothing
                }
            , indexedBy = Nothing
            }
        , where_ = Just
            ( Expression'GreaterThan
                ( Expression'Column
                    ( Namespaced
                        { namespace = Nothing
                        , value = "f1"
                        }
                    )
                )
                ( Expression'LiteralValue
                    ( Number "7" )
                )
            )
        , returning = Nothing
        }
    )