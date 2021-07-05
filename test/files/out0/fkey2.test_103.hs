Statement'Delete
    ( DeleteStatement
        { commonTableExpressions = Nothing
        , table = QualifiedTableName
            { name = Aliased
                { value = Namespaced
                    { namespace = Nothing
                    , value = "self"
                    }
                , alias = Nothing
                }
            , indexedBy = Nothing
            }
        , where_ = Nothing
        , returning = Nothing
        }
    )