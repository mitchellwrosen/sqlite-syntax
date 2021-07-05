Statement'Delete
    ( DeleteStatement
        { commonTableExpressions = Nothing
        , table = QualifiedTableName
            { name = Aliased
                { value = Namespaced
                    { namespace = Nothing
                    , value = "ft"
                    }
                , alias = Nothing
                }
            , indexedBy = Nothing
            }
        , where_ = Just
            ( Expression'GreaterThanOrEquals
                ( Expression'Column
                    ( Namespaced
                        { namespace = Nothing
                        , value = "docid"
                        }
                    )
                )
                ( Expression'LiteralValue
                    ( Number "32" )
                )
            )
        , returning = Nothing
        }
    )