Statement'Delete
    ( DeleteStatement
        { commonTableExpressions = Nothing
        , table = QualifiedTableName
            { name = Aliased
                { value = Namespaced
                    { namespace = Nothing
                    , value = "t1_segments"
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
                        , value = "blockid"
                        }
                    )
                )
                ( Expression'Column
                    ( Namespaced
                        { namespace = Nothing
                        , value = "left_child"
                        }
                    )
                )
            )
        , returning = Nothing
        }
    )