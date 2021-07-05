Statement'Delete
    ( DeleteStatement
        { commonTableExpressions = Nothing
        , table = QualifiedTableName
            { name = Aliased
                { value = Namespaced
                    { namespace = Nothing
                    , value = "filler"
                    }
                , alias = Nothing
                }
            , indexedBy = Nothing
            }
        , where_ = Just
            ( Expression'LessThanOrEquals
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
                                            { value = Expression'FunctionCall
                                                ( FunctionCallExpression
                                                    { call = FunctionCall
                                                        { name = Namespaced
                                                            { namespace = Nothing
                                                            , value = "MAX"
                                                            }
                                                        , arguments = FunctionArguments'Arguments
                                                            [ Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "rowid"
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
                                                        , value = "filler"
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
                        , limit = Just
                            ( Limit
                                { limit = Expression'LiteralValue
                                    ( Number "20" )
                                , offset = Nothing
                                }
                            )
                        }
                    )
                )
            )
        , returning = Nothing
        }
    )