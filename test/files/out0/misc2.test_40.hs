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
                ( Expression'Column
                    ( Namespaced
                        { namespace = Nothing
                        , value = "rowid"
                        }
                    )
                )
                ( Expression'Column
                    ( Namespaced
                        { namespace = Nothing
                        , value = "rowid"
                        }
                    )
                )
            )
        , returning = Nothing
        }
    )