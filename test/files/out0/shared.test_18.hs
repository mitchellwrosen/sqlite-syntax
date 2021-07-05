Statement'Delete
    ( DeleteStatement
        { commonTableExpressions = Nothing
        , table = QualifiedTableName
            { name = Aliased
                { value = Namespaced
                    { namespace = Nothing
                    , value = "seq"
                    }
                , alias = Nothing
                }
            , indexedBy = Nothing
            }
        , where_ = Just
            ( Expression'Equals
                ( Expression'Column
                    ( Namespaced
                        { namespace = Nothing
                        , value = "i"
                        }
                    )
                )
                ( Expression'Parameter
                    ( Parameter'Named "i" )
                )
            )
        , returning = Nothing
        }
    )