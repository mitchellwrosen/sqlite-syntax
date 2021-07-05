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
        , where_ = Just
            ( Expression'LessThan
                ( Expression'Column
                    ( Namespaced
                        { namespace = Nothing
                        , value = "x"
                        }
                    )
                )
                ( Expression'Multiply
                    ( Expression'LiteralValue
                        ( Number "0.5" )
                    )
                    ( Expression'Subquery
                        ( SelectStatement
                            { commonTableExpressions = Nothing
                            , select = CompoundSelect
                                ( SelectCore'Select
                                    ( Select
                                        { distinct = False
                                        , columns = ResultColumn'Expression
                                            ( Aliased
                                                { value = Expression'FunctionCall
                                                    ( FunctionCallExpression
                                                        { call = FunctionCall
                                                            { name = Namespaced
                                                                { namespace = Nothing
                                                                , value = "max"
                                                                }
                                                            , arguments = FunctionArguments'Arguments
                                                                [ Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "x"
                                                                        }
                                                                    )
                                                                ]
                                                            }
                                                        , filter = Nothing
                                                        , over = Nothing
                                                        }
                                                    )
                                                , alias = Nothing
                                                }
                                            ) :| []
                                        , from = Just
                                            ( Table
                                                ( QualifiedTableName
                                                    { name = Aliased
                                                        { value = Namespaced
                                                            { namespace = Nothing
                                                            , value = "t2"
                                                            }
                                                        , alias = Nothing
                                                        }
                                                    , indexedBy = Nothing
                                                    }
                                                )
                                            )
                                        , where_ = Nothing
                                        , groupBy = Nothing
                                        , window = Nothing
                                        }
                                    )
                                )
                            , orderBy = Nothing
                            , limit = Nothing
                            }
                        )
                    )
                )
            )
        , returning = Nothing
        }
    )