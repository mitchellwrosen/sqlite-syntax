Statement'Delete
    ( DeleteStatement
        { commonTableExpressions = Nothing
        , table = QualifiedTableName
            { name = Aliased
                { value = Namespaced
                    { namespace = Nothing
                    , value = "a"
                    }
                , alias = Nothing
                }
            , indexedBy = Nothing
            }
        , where_ = Just
            ( Expression'Column
                ( Namespaced
                    { namespace = Nothing
                    , value = "oid"
                    }
                )
            )
        , returning = Nothing
        }
    )