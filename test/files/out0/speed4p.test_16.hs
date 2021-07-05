Statement'Delete
    ( DeleteStatement
        { commonTableExpressions = Nothing
        , table = QualifiedTableName
            { name = Aliased
                { value = Namespaced
                    { namespace = Nothing
                    , value = "t4"
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
                        , value = "ii"
                        }
                    )
                )
            )
        , returning = Nothing
        }
    )