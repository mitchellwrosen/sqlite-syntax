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
            ( Expression'InValues
                ( InValuesExpression
                    { expression = Expression'Column
                        ( Namespaced
                            { namespace = Nothing
                            , value = "b"
                            }
                        )
                    , values =
                        [ Expression'LiteralValue
                            ( Number "2" )
                        , Expression'LiteralValue
                            ( Number "4" )
                        , Expression'LiteralValue
                            ( Number "6" )
                        , Expression'LiteralValue
                            ( Number "8" )
                        ]
                    }
                )
            )
        , returning = Nothing
        }
    )