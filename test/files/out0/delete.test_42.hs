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
        , where_ = Nothing
        , returning = Nothing
        }
    )