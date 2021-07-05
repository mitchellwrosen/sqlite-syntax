Statement'Delete
    ( DeleteStatement
        { commonTableExpressions = Nothing
        , table = QualifiedTableName
            { name = Aliased
                { value = Namespaced
                    { namespace = Nothing
                    , value = "t2"
                    }
                , alias = Nothing
                }
            , indexedBy = Nothing
            }
        , where_ = Nothing
        , returning = Nothing
        }
    )