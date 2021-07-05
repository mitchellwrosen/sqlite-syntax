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
            ( Expression'GreaterThan
                ( Expression'Column
                    ( Namespaced
                        { namespace = Nothing
                        , value = "rowid"
                        }
                    )
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
                                            { value = Expression'Divide
                                                ( Expression'FunctionCall
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
                                                                        , value = "a"
                                                                        }
                                                                    )
                                                                ]
                                                            }
                                                        , filter = Nothing
                                                        , over = Nothing
                                                        }
                                                    )
                                                )
                                                ( Expression'LiteralValue
                                                    ( Number "2" )
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
                                                        , value = "t1"
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
        , returning = Nothing
        }
    )