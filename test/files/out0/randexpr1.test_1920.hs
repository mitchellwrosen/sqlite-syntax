Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'BitwiseAnd
                                ( Expression'BitwiseAnd
                                    ( Expression'Negate
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
                                    )
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
                                                                                { value = Expression'Multiply
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
                                                                                    ( Expression'Subquery
                                                                                        ( SelectStatement
                                                                                            { commonTableExpressions = Nothing
                                                                                            , select = CompoundSelect
                                                                                                ( SelectCore'Select
                                                                                                    ( Select
                                                                                                        { distinct = False
                                                                                                        , columns = ResultColumn'Expression
                                                                                                            ( Aliased
                                                                                                                { value = Expression'Plus
                                                                                                                    ( Expression'FunctionCall
                                                                                                                        ( FunctionCallExpression
                                                                                                                            { call = FunctionCall
                                                                                                                                { name = Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "min"
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
                                                                                                                    ( Expression'Multiply
                                                                                                                        ( Expression'FunctionCall
                                                                                                                            ( FunctionCallExpression
                                                                                                                                { call = FunctionCall
                                                                                                                                    { name = Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "min"
                                                                                                                                        }
                                                                                                                                    , arguments = FunctionArguments'Arguments
                                                                                                                                        [ Expression'Multiply
                                                                                                                                            ( Expression'Negate
                                                                                                                                                ( Expression'Subquery
                                                                                                                                                    ( SelectStatement
                                                                                                                                                        { commonTableExpressions = Nothing
                                                                                                                                                        , select = CompoundSelect
                                                                                                                                                            ( SelectCore'Select
                                                                                                                                                                ( Select
                                                                                                                                                                    { distinct = False
                                                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                                                        ( Aliased
                                                                                                                                                                            { value = Expression'Negate
                                                                                                                                                                                ( Expression'Case
                                                                                                                                                                                    ( CaseExpression
                                                                                                                                                                                        { base = Just
                                                                                                                                                                                            ( Expression'FunctionCall
                                                                                                                                                                                                ( FunctionCallExpression
                                                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                            , value = "min"
                                                                                                                                                                                                            }
                                                                                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                            [ Expression'LiteralValue
                                                                                                                                                                                                                ( Number "13" )
                                                                                                                                                                                                            ]
                                                                                                                                                                                                        }
                                                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                                                    , over = Nothing
                                                                                                                                                                                                    }
                                                                                                                                                                                                )
                                                                                                                                                                                            )
                                                                                                                                                                                        , cases =
                                                                                                                                                                                            ( Expression'Negate
                                                                                                                                                                                                ( Expression'FunctionCall
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
                                                                                                                                                                                                                                        , value = "b"
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
                                                                                                                                                                                                )
                                                                                                                                                                                            , Expression'AggregateDistinctFunctionCall
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
                                                                                                                                                        , orderBy = Nothing
                                                                                                                                                        , limit = Nothing
                                                                                                                                                        }
                                                                                                                                                    )
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
                                                                                                                                                                        { value = Expression'Negate
                                                                                                                                                                            ( Expression'Negate
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
                                                                                                                                        ]
                                                                                                                                    }
                                                                                                                                , filter = Nothing
                                                                                                                                , over = Nothing
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                        ( Expression'Negate
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
                                                                                                                { value = Expression'Multiply
                                                                                                                    ( Expression'Multiply
                                                                                                                        ( Expression'Case
                                                                                                                            ( CaseExpression
                                                                                                                                { base = Just
                                                                                                                                    ( Expression'Case
                                                                                                                                        ( CaseExpression
                                                                                                                                            { base = Nothing
                                                                                                                                            , cases =
                                                                                                                                                ( Expression'LessThan
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
                                                                                                                                                    ( Expression'Column
                                                                                                                                                        ( Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "f"
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                , Expression'Column
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "c"
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                ) :| []
                                                                                                                                            , else_ = Expression'LiteralValue
                                                                                                                                                ( Number "19" )
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                , cases =
                                                                                                                                    ( Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "f"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    , Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "b"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    ) :| []
                                                                                                                                , else_ = Expression'Negate
                                                                                                                                    ( Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "c"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                        ( Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "a"
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
                                                    , Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "b"
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
                                        , value = "a"
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
                        ( Expression'Or
                            ( Expression'Not
                                ( Expression'Between
                                    ( Expression'Multiply
                                        ( Expression'LiteralValue
                                            ( Number "19" )
                                        )
                                        ( Expression'Case
                                            ( CaseExpression
                                                { base = Nothing
                                                , cases =
                                                    ( Expression'Exists
                                                        ( SelectStatement
                                                            { commonTableExpressions = Nothing
                                                            , select = CompoundSelect
                                                                ( SelectCore'Select
                                                                    ( Select
                                                                        { distinct = False
                                                                        , columns = ResultColumn'Expression
                                                                            ( Aliased
                                                                                { value = Expression'LiteralValue
                                                                                    ( Number "1" )
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
                                                                            ( Expression'Or
                                                                                ( Expression'LessThan
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "e"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                    ( Expression'BitwiseOr
                                                                                        ( Expression'LiteralValue
                                                                                            ( Number "13" )
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
                                                                                ( Expression'Equals
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
                                                                                    ( Expression'LiteralValue
                                                                                        ( Number "19" )
                                                                                    )
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
                                                    , Expression'Column
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
                                                    ) :|
                                                    [
                                                        ( Expression'Or
                                                            ( Expression'Or
                                                                ( Expression'And
                                                                    ( Expression'LessThanOrEquals
                                                                        ( Expression'Case
                                                                            ( CaseExpression
                                                                                { base = Nothing
                                                                                , cases =
                                                                                    ( Expression'Exists
                                                                                        ( SelectStatement
                                                                                            { commonTableExpressions = Nothing
                                                                                            , select = CompoundSelect
                                                                                                ( SelectCore'Select
                                                                                                    ( Select
                                                                                                        { distinct = False
                                                                                                        , columns = ResultColumn'Expression
                                                                                                            ( Aliased
                                                                                                                { value = Expression'LiteralValue
                                                                                                                    ( Number "1" )
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
                                                                                                                            , value = "c"
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
                                                                                                                                                { value = Expression'LiteralValue
                                                                                                                                                    ( Number "11" )
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
                                                                                    , Expression'Multiply
                                                                                        ( Expression'LiteralValue
                                                                                            ( Number "13" )
                                                                                        )
                                                                                        ( Expression'Column
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
                                                                                        )
                                                                                    ) :| []
                                                                                , else_ = Expression'LiteralValue
                                                                                    ( Number "19" )
                                                                                }
                                                                            )
                                                                        )
                                                                        ( Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "c"
                                                                                }
                                                                            )
                                                                        )
                                                                    )
                                                                    ( Expression'Between
                                                                        ( Expression'Column
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
                                                                        )
                                                                        ( Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "b"
                                                                                }
                                                                            )
                                                                        )
                                                                        ( Expression'LiteralValue
                                                                            ( Number "13" )
                                                                        )
                                                                    )
                                                                )
                                                                ( Expression'Not
                                                                    ( Expression'Between
                                                                        ( Expression'LiteralValue
                                                                            ( Number "11" )
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
                                                                        ( Expression'LiteralValue
                                                                            ( Number "13" )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                            ( Expression'Between
                                                                ( Expression'LiteralValue
                                                                    ( Number "17" )
                                                                )
                                                                ( Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "b"
                                                                        }
                                                                    )
                                                                )
                                                                ( Expression'LiteralValue
                                                                    ( Number "13" )
                                                                )
                                                            )
                                                        , Expression'Plus
                                                            ( Expression'Column
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
                                                            )
                                                            ( Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "a"
                                                                    }
                                                                )
                                                            )
                                                        )
                                                    ]
                                                , else_ = Expression'LiteralValue
                                                    ( Number "17" )
                                                }
                                            )
                                        )
                                    )
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
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
                                )
                            )
                            ( Expression'Not
                                ( Expression'Between
                                    ( Expression'Column
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
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "13" )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "17" )
                                    )
                                )
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