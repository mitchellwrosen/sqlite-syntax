Statement'Delete
    ( DeleteStatement
        { commonTableExpressions = Nothing
        , table = QualifiedTableName
            { name = Aliased
                { value = Namespaced
                    { namespace = Nothing
                    , value = "t3"
                    }
                , alias = Nothing
                }
            , indexedBy = Nothing
            }
        , where_ = Just
            ( Expression'LiteralValue
                ( Number "1" )
            )
        , returning = Nothing
        }
    )