Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Not
                                ( Expression'InSubquery
                                    ( InSubqueryExpression
                                        { expression = Expression'LiteralValue
                                            ( Number "2" )
                                        , subquery = SelectStatement
                                            { commonTableExpressions = Nothing
                                            , select = CompoundSelect
                                                ( SelectCore'Select
                                                    ( Select
                                                        { distinct = False
                                                        , columns = ResultColumn'Expression
                                                            ( Aliased
                                                                { value = Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "a"
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
                                                                            , value = "t7"
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
                                        }
                                    )
                                )
                            , alias = Nothing
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Not
                                    ( Expression'InSubquery
                                        ( InSubqueryExpression
                                            { expression = Expression'LiteralValue
                                                ( Number "6" )
                                            , subquery = SelectStatement
                                                { commonTableExpressions = Nothing
                                                , select = CompoundSelect
                                                    ( SelectCore'Select
                                                        ( Select
                                                            { distinct = False
                                                            , columns = ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "a"
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
                                                                                , value = "t7"
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
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Not
                                    ( Expression'InSubquery
                                        ( InSubqueryExpression
                                            { expression = Expression'LiteralValue
                                                ( Number "2" )
                                            , subquery = SelectStatement
                                                { commonTableExpressions = Nothing
                                                , select = CompoundSelect
                                                    ( SelectCore'Select
                                                        ( Select
                                                            { distinct = False
                                                            , columns = ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "b"
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
                                                                                , value = "t7"
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
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Not
                                    ( Expression'InSubquery
                                        ( InSubqueryExpression
                                            { expression = Expression'LiteralValue
                                                ( Number "6" )
                                            , subquery = SelectStatement
                                                { commonTableExpressions = Nothing
                                                , select = CompoundSelect
                                                    ( SelectCore'Select
                                                        ( Select
                                                            { distinct = False
                                                            , columns = ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "b"
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
                                                                                , value = "t7"
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
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Not
                                    ( Expression'InSubquery
                                        ( InSubqueryExpression
                                            { expression = Expression'LiteralValue
                                                ( Number "2" )
                                            , subquery = SelectStatement
                                                { commonTableExpressions = Nothing
                                                , select = CompoundSelect
                                                    ( SelectCore'Select
                                                        ( Select
                                                            { distinct = False
                                                            , columns = ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "c"
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
                                                                                , value = "t7"
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
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Not
                                    ( Expression'InSubquery
                                        ( InSubqueryExpression
                                            { expression = Expression'LiteralValue
                                                ( Number "6" )
                                            , subquery = SelectStatement
                                                { commonTableExpressions = Nothing
                                                , select = CompoundSelect
                                                    ( SelectCore'Select
                                                        ( Select
                                                            { distinct = False
                                                            , columns = ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "c"
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
                                                                                , value = "t7"
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
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        ]
                    , from = Nothing
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