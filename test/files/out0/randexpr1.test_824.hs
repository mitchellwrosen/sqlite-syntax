Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'BitwiseOr
                                ( Expression'Minus
                                    ( Expression'LiteralValue
                                        ( Number "13" )
                                    )
                                    ( Expression'Negate
                                        ( Expression'Divide
                                            ( Expression'FunctionCall
                                                ( FunctionCallExpression
                                                    { call = FunctionCall
                                                        { name = Namespaced
                                                            { namespace = Nothing
                                                            , value = "abs"
                                                            }
                                                        , arguments = FunctionArguments'Arguments
                                                            [ Expression'BitwiseOr
                                                                ( Expression'FunctionCall
                                                                    ( FunctionCallExpression
                                                                        { call = FunctionCall
                                                                            { name = Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "coalesce"
                                                                                }
                                                                            , arguments = FunctionArguments'Arguments
                                                                                [ Expression'Subquery
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
                                                                                                                            [ Expression'LiteralValue
                                                                                                                                ( Number "17" )
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
                                                                                                                        , value = "t1"
                                                                                                                        }
                                                                                                                    , alias = Nothing
                                                                                                                    }
                                                                                                                , indexedBy = Nothing
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                    , where_ = Just
                                                                                                        ( Expression'InSubquery
                                                                                                            ( InSubqueryExpression
                                                                                                                { expression = Expression'Multiply
                                                                                                                    ( Expression'Column
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "f"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                    ( Expression'Column
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "b"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                , subquery = SelectStatement
                                                                                                                    { commonTableExpressions = Nothing
                                                                                                                    , select = Union
                                                                                                                        ( CompoundSelect
                                                                                                                            ( SelectCore'Select
                                                                                                                                ( Select
                                                                                                                                    { distinct = False
                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                        ( Aliased
                                                                                                                                            { value = Expression'Multiply
                                                                                                                                                ( Expression'FunctionCall
                                                                                                                                                    ( FunctionCallExpression
                                                                                                                                                        { call = FunctionCall
                                                                                                                                                            { name = Namespaced
                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                , value = "count"
                                                                                                                                                                }
                                                                                                                                                            , arguments = FunctionArguments'Wildcard
                                                                                                                                                            }
                                                                                                                                                        , filter = Nothing
                                                                                                                                                        , over = Nothing
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                                ( Expression'Case
                                                                                                                                                    ( CaseExpression
                                                                                                                                                        { base = Just
                                                                                                                                                            ( Expression'Plus
                                                                                                                                                                ( Expression'Negate
                                                                                                                                                                    ( Expression'FunctionCall
                                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                                { name = Namespaced
                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                    , value = "min"
                                                                                                                                                                                    }
                                                                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                                                                    [ Expression'BitwiseNegate
                                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Just
                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                        , value = "t1"
                                                                                                                                                                                                        }
                                                                                                                                                                                                    )
                                                                                                                                                                                                , value = "f"
                                                                                                                                                                                                }
                                                                                                                                                                                            )
                                                                                                                                                                                        )
                                                                                                                                                                                    ]
                                                                                                                                                                                }
                                                                                                                                                                            , filter = Nothing
                                                                                                                                                                            , over = Nothing
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                                ( Expression'FunctionCall
                                                                                                                                                                    ( FunctionCallExpression
                                                                                                                                                                        { call = FunctionCall
                                                                                                                                                                            { name = Namespaced
                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                , value = "min"
                                                                                                                                                                                }
                                                                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                                                                [ Expression'Plus
                                                                                                                                                                                    ( Expression'FunctionCall
                                                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                                                { name = Namespaced
                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                    , value = "coalesce"
                                                                                                                                                                                                    }
                                                                                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                    [ Expression'Subquery
                                                                                                                                                                                                        ( SelectStatement
                                                                                                                                                                                                            { commonTableExpressions = Nothing
                                                                                                                                                                                                            , select = CompoundSelect
                                                                                                                                                                                                                ( SelectCore'Select
                                                                                                                                                                                                                    ( Select
                                                                                                                                                                                                                        { distinct = False
                                                                                                                                                                                                                        , columns = ResultColumn'Expression
                                                                                                                                                                                                                            ( Aliased
                                                                                                                                                                                                                                { value = Expression'Negate
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
                                                                                                                                                                                                                                                                                , value = "min"
                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                                                                [ Expression'LiteralValue
                                                                                                                                                                                                                                                                                    ( Number "17" )
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
                                                                                                                                                                                                                        , where_ = Just
                                                                                                                                                                                                                            ( Expression'InSubquery
                                                                                                                                                                                                                                ( InSubqueryExpression
                                                                                                                                                                                                                                    { expression = Expression'Column
                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                            , value = "b"
                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                    , subquery = SelectStatement
                                                                                                                                                                                                                                        { commonTableExpressions = Nothing
                                                                                                                                                                                                                                        , select = Union
                                                                                                                                                                                                                                            ( CompoundSelect
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
                                                                                                                                                                                                                                                                                , value = "abs"
                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                                                                [ Expression'FunctionCall
                                                                                                                                                                                                                                                                                    ( FunctionCallExpression
                                                                                                                                                                                                                                                                                        { call = FunctionCall
                                                                                                                                                                                                                                                                                            { name = Namespaced
                                                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                                                , value = "min"
                                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                                                                                [ Expression'Column
                                                                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                                                        , value = "f"
                                                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                        , filter = Nothing
                                                                                                                                                                                                                                                                                        , over = Nothing
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
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                            ( SelectCore'Select
                                                                                                                                                                                                                                                ( Select
                                                                                                                                                                                                                                                    { distinct = False
                                                                                                                                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                                                                                                                                        ( Aliased
                                                                                                                                                                                                                                                            { value = Expression'Negate
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
                                                                                                                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                                        , value = "e"
                                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                        , filter = Nothing
                                                                                                                                                                                                                                                                        , over = Nothing
                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                    )
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
                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                            )
                                                                                                                                                                                                                        , groupBy = Nothing
                                                                                                                                                                                                                        , window = Nothing
                                                                                                                                                                                                                        }
                                                                                                                                                                                                                    )
                                                                                                                                                                                                                )
                                                                                                                                                                                                            , orderBy = Nothing
                                                                                                                                                                                                            , limit = Nothing
                                                                                                                                                                                                            }
                                                                                                                                                                                                        )
                                                                                                                                                                                                    , Expression'LiteralValue
                                                                                                                                                                                                        ( Number "17" )
                                                                                                                                                                                                    ]
                                                                                                                                                                                                }
                                                                                                                                                                                            , filter = Nothing
                                                                                                                                                                                            , over = Nothing
                                                                                                                                                                                            }
                                                                                                                                                                                        )
                                                                                                                                                                                    )
                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                            , value = "d"
                                                                                                                                                                                            }
                                                                                                                                                                                        )
                                                                                                                                                                                    )
                                                                                                                                                                                ]
                                                                                                                                                                            }
                                                                                                                                                                        , filter = Nothing
                                                                                                                                                                        , over = Nothing
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                        , cases =
                                                                                                                                                            ( Expression'AggregateDistinctFunctionCall
                                                                                                                                                                ( AggregateDistinctFunctionCallExpression
                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "count"
                                                                                                                                                                            }
                                                                                                                                                                        , arguments = Identity
                                                                                                                                                                            ( Expression'Column
                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                    , value = "a"
                                                                                                                                                                                    }
                                                                                                                                                                                )
                                                                                                                                                                            )
                                                                                                                                                                        }
                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            , Expression'FunctionCall
                                                                                                                                                                ( FunctionCallExpression
                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "abs"
                                                                                                                                                                            }
                                                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                                                            [ Expression'FunctionCall
                                                                                                                                                                                ( FunctionCallExpression
                                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                            , value = "count"
                                                                                                                                                                                            }
                                                                                                                                                                                        , arguments = FunctionArguments'Wildcard
                                                                                                                                                                                        }
                                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                                    , over = Nothing
                                                                                                                                                                                    }
                                                                                                                                                                                )
                                                                                                                                                                            ]
                                                                                                                                                                        }
                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                    , over = Nothing
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            ) :| []
                                                                                                                                                        , else_ = Expression'FunctionCall
                                                                                                                                                            ( FunctionCallExpression
                                                                                                                                                                { call = FunctionCall
                                                                                                                                                                    { name = Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "count"
                                                                                                                                                                        }
                                                                                                                                                                    , arguments = FunctionArguments'Wildcard
                                                                                                                                                                    }
                                                                                                                                                                , filter = Nothing
                                                                                                                                                                , over = Nothing
                                                                                                                                                                }
                                                                                                                                                            )
                                                                                                                                                        }
                                                                                                                                                    )
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
                                                                                                                        )
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
                                                                                                                                                        , value = "min"
                                                                                                                                                        }
                                                                                                                                                    , arguments = FunctionArguments'Arguments
                                                                                                                                                        [ Expression'Column
                                                                                                                                                            ( Namespaced
                                                                                                                                                                { namespace = Just
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "t1"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                , value = "c"
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
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                    , groupBy = Nothing
                                                                                                    , window = Nothing
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                        , orderBy = Nothing
                                                                                        , limit = Nothing
                                                                                        }
                                                                                    )
                                                                                , Expression'FunctionCall
                                                                                    ( FunctionCallExpression
                                                                                        { call = FunctionCall
                                                                                            { name = Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "coalesce"
                                                                                                }
                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                [ Expression'Subquery
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
                                                                                                                                                    , value = "f"
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
                                                                                                                                        , value = "t1"
                                                                                                                                        }
                                                                                                                                    , alias = Nothing
                                                                                                                                    }
                                                                                                                                , indexedBy = Nothing
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                    , where_ = Just
                                                                                                                        ( Expression'InSubquery
                                                                                                                            ( InSubqueryExpression
                                                                                                                                { expression = Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "d"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                , subquery = SelectStatement
                                                                                                                                    { commonTableExpressions = Nothing
                                                                                                                                    , select = Union
                                                                                                                                        ( CompoundSelect
                                                                                                                                            ( SelectCore'Select
                                                                                                                                                ( Select
                                                                                                                                                    { distinct = False
                                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                                        ( Aliased
                                                                                                                                                            { value = Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Just
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "t1"
                                                                                                                                                                            }
                                                                                                                                                                        )
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
                                                                                                                                        )
                                                                                                                                        ( SelectCore'Select
                                                                                                                                            ( Select
                                                                                                                                                { distinct = False
                                                                                                                                                , columns = ResultColumn'Expression
                                                                                                                                                    ( Aliased
                                                                                                                                                        { value = Expression'Column
                                                                                                                                                            ( Namespaced
                                                                                                                                                                { namespace = Just
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "t1"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                , value = "e"
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
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                    , groupBy = Nothing
                                                                                                                    , window = Nothing
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                        , orderBy = Nothing
                                                                                                        , limit = Nothing
                                                                                                        }
                                                                                                    )
                                                                                                , Expression'LiteralValue
                                                                                                    ( Number "17" )
                                                                                                ]
                                                                                            }
                                                                                        , filter = Nothing
                                                                                        , over = Nothing
                                                                                        }
                                                                                    )
                                                                                ]
                                                                            }
                                                                        , filter = Nothing
                                                                        , over = Nothing
                                                                        }
                                                                    )
                                                                )
                                                                ( Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Just
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "t1"
                                                                                }
                                                                            )
                                                                        , value = "a"
                                                                        }
                                                                    )
                                                                )
                                                            ]
                                                        }
                                                    , filter = Nothing
                                                    , over = Nothing
                                                    }
                                                )
                                            )
                                            ( Expression'FunctionCall
                                                ( FunctionCallExpression
                                                    { call = FunctionCall
                                                        { name = Namespaced
                                                            { namespace = Nothing
                                                            , value = "abs"
                                                            }
                                                        , arguments = FunctionArguments'Arguments
                                                            [ Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "c"
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    , filter = Nothing
                                                    , over = Nothing
                                                    }
                                                )
                                            )
                                        )
                                    )
                                )
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Just
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "t1"
                                                }
                                            )
                                        , value = "f"
                                        }
                                    )
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
                    , where_ = Just
                        ( Expression'InSubquery
                            ( InSubqueryExpression
                                { expression = Expression'Plus
                                    ( Expression'Plus
                                        ( Expression'Divide
                                            ( Expression'FunctionCall
                                                ( FunctionCallExpression
                                                    { call = FunctionCall
                                                        { name = Namespaced
                                                            { namespace = Nothing
                                                            , value = "abs"
                                                            }
                                                        , arguments = FunctionArguments'Arguments
                                                            [ Expression'Plus
                                                                ( Expression'Multiply
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Just
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "t1"
                                                                                    }
                                                                                )
                                                                            , value = "f"
                                                                            }
                                                                        )
                                                                    )
                                                                    ( Expression'LiteralValue
                                                                        ( Number "11" )
                                                                    )
                                                                )
                                                                ( Expression'Negate
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "f"
                                                                            }
                                                                        )
                                                                    )
                                                                )
                                                            ]
                                                        }
                                                    , filter = Nothing
                                                    , over = Nothing
                                                    }
                                                )
                                            )
                                            ( Expression'FunctionCall
                                                ( FunctionCallExpression
                                                    { call = FunctionCall
                                                        { name = Namespaced
                                                            { namespace = Nothing
                                                            , value = "abs"
                                                            }
                                                        , arguments = FunctionArguments'Arguments
                                                            [ Expression'BitwiseOr
                                                                ( Expression'LiteralValue
                                                                    ( Number "19" )
                                                                )
                                                                ( Expression'Plus
                                                                    ( Expression'Multiply
                                                                        ( Expression'Divide
                                                                            ( Expression'FunctionCall
                                                                                ( FunctionCallExpression
                                                                                    { call = FunctionCall
                                                                                        { name = Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "abs"
                                                                                            }
                                                                                        , arguments = FunctionArguments'Arguments
                                                                                            [ Expression'Divide
                                                                                                ( Expression'FunctionCall
                                                                                                    ( FunctionCallExpression
                                                                                                        { call = FunctionCall
                                                                                                            { name = Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "abs"
                                                                                                                }
                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                [ Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "f"
                                                                                                                        }
                                                                                                                    )
                                                                                                                ]
                                                                                                            }
                                                                                                        , filter = Nothing
                                                                                                        , over = Nothing
                                                                                                        }
                                                                                                    )
                                                                                                )
                                                                                                ( Expression'FunctionCall
                                                                                                    ( FunctionCallExpression
                                                                                                        { call = FunctionCall
                                                                                                            { name = Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "abs"
                                                                                                                }
                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                [ Expression'LiteralValue
                                                                                                                    ( Number "17" )
                                                                                                                ]
                                                                                                            }
                                                                                                        , filter = Nothing
                                                                                                        , over = Nothing
                                                                                                        }
                                                                                                    )
                                                                                                )
                                                                                            ]
                                                                                        }
                                                                                    , filter = Nothing
                                                                                    , over = Nothing
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Expression'FunctionCall
                                                                                ( FunctionCallExpression
                                                                                    { call = FunctionCall
                                                                                        { name = Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "abs"
                                                                                            }
                                                                                        , arguments = FunctionArguments'Arguments
                                                                                            [ Expression'Case
                                                                                                ( CaseExpression
                                                                                                    { base = Nothing
                                                                                                    , cases =
                                                                                                        ( Expression'Not
                                                                                                            ( Expression'InValues
                                                                                                                ( InValuesExpression
                                                                                                                    { expression = Expression'Column
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "a"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    , values =
                                                                                                                        [ Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Just
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "t1"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                , value = "d"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        , Expression'BitwiseOr
                                                                                                                            ( Expression'Minus
                                                                                                                                ( Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "c"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                )
                                                                                                                                ( Expression'Multiply
                                                                                                                                    ( Expression'BitwiseNegate
                                                                                                                                        ( Expression'Column
                                                                                                                                            ( Namespaced
                                                                                                                                                { namespace = Nothing
                                                                                                                                                , value = "c"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                    ( Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "f"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                )
                                                                                                                            )
                                                                                                                            ( Expression'Plus
                                                                                                                                ( Expression'Minus
                                                                                                                                    ( Expression'Negate
                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                            ( Number "13" )
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                    ( Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Just
                                                                                                                                                ( Namespaced
                                                                                                                                                    { namespace = Nothing
                                                                                                                                                    , value = "t1"
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            , value = "b"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                )
                                                                                                                                ( Expression'Negate
                                                                                                                                    ( Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "d"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                )
                                                                                                                            )
                                                                                                                        , Expression'LiteralValue
                                                                                                                            ( Number "13" )
                                                                                                                        ]
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                        , Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Just
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "t1"
                                                                                                                        }
                                                                                                                    )
                                                                                                                , value = "f"
                                                                                                                }
                                                                                                            )
                                                                                                        ) :|
                                                                                                        [
                                                                                                            ( Expression'Equals
                                                                                                                ( Expression'LiteralValue
                                                                                                                    ( Number "11" )
                                                                                                                )
                                                                                                                ( Expression'LiteralValue
                                                                                                                    ( Number "11" )
                                                                                                                )
                                                                                                            , Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "c"
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                        ]
                                                                                                    , else_ = Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "d"
                                                                                                            }
                                                                                                        )
                                                                                                    }
                                                                                                )
                                                                                            ]
                                                                                        }
                                                                                    , filter = Nothing
                                                                                    , over = Nothing
                                                                                    }
                                                                                )
                                                                            )
                                                                        )
                                                                        ( Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "d"
                                                                                }
                                                                            )
                                                                        )
                                                                    )
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Just
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "t1"
                                                                                    }
                                                                                )
                                                                            , value = "c"
                                                                            }
                                                                        )
                                                                    )
                                                                )
                                                            ]
                                                        }
                                                    , filter = Nothing
                                                    , over = Nothing
                                                    }
                                                )
                                            )
                                        )
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "e"
                                                }
                                            )
                                        )
                                    )
                                    ( Expression'Negate
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Just
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "t1"
                                                        }
                                                    )
                                                , value = "d"
                                                }
                                            )
                                        )
                                    )
                                , subquery = SelectStatement
                                    { commonTableExpressions = Nothing
                                    , select = Union
                                        ( CompoundSelect
                                            ( SelectCore'Select
                                                ( Select
                                                    { distinct = False
                                                    , columns = ResultColumn'Expression
                                                        ( Aliased
                                                            { value = Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Just
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "t1"
                                                                            }
                                                                        )
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
                                        )
                                        ( SelectCore'Select
                                            ( Select
                                                { distinct = False
                                                , columns = ResultColumn'Expression
                                                    ( Aliased
                                                        { value = Expression'LiteralValue
                                                            ( Number "19" )
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
                                }
                            )
                        )
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )