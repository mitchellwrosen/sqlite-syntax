Statement'Delete
    ( DeleteStatement
        { commonTableExpressions = Nothing
        , table = QualifiedTableName
            { name = Aliased
                { value = Namespaced
                    { namespace = Nothing
                    , value = "p"
                    }
                , alias = Nothing
                }
            , indexedBy = Nothing
            }
        , where_ = Nothing
        , returning = Nothing
        }
    )