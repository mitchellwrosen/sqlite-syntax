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
        , where_ = Just
            ( Expression'FunctionCall
                ( FunctionCallExpression
                    { call = FunctionCall
                        { name = Namespaced
                            { namespace = Nothing
                            , value = "xyzzy"
                            }
                        , arguments = FunctionArguments'Arguments
                            [ Expression'Plus
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "f1"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "4" )
                                )
                            ]
                        }
                    , filter = Nothing
                    , over = Nothing
                    }
                )
            )
        , returning = Nothing
        }
    )