Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Case
                                ( CaseExpression
                                    { base = Nothing
                                    , cases =
                                        ( Expression'NotEquals
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
                                                                                , where_ = Just
                                                                                    ( Expression'Or
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
                                                                                                                ( Expression'LessThanOrEquals
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
                                                                                                                    ( Expression'Minus
                                                                                                                        ( Expression'Minus
                                                                                                                            ( Expression'Minus
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
                                                                                                                                ( Expression'Multiply
                                                                                                                                    ( Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "e"
                                                                                                                                            }
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
                                                                                                                                                                                                        , value = "a"
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
                                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                    }
                                                                                                                                                                                                )
                                                                                                                                                                                            , value = "e"
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
                                                                                                                                                                                                                                , value = "min"
                                                                                                                                                                                                                                }
                                                                                                                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                [ Expression'Plus
                                                                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                                                                        ( Number "13" )
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                                                                        ( Number "13" )
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
                                                                                                                                                                                                                ( Expression'Minus
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
                                                                                                                                                                                                                                }
                                                                                                                                                                                                                            , filter = Nothing
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                        )
                                                                                                                                                                                                                    )
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
                                                                                        )
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
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "a"
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
                                                , value = "d"
                                                }
                                            )
                                        ) :|
                                        [
                                            ( Expression'Not
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
                                                                        ( Expression'InValues
                                                                            ( InValuesExpression
                                                                                { expression = Expression'Column
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
                                                                                , values =
                                                                                    [ Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "c"
                                                                                            }
                                                                                        )
                                                                                    , Expression'LiteralValue
                                                                                        ( Number "19" )
                                                                                    , Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "f"
                                                                                            }
                                                                                        )
                                                                                    ]
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
                                            )
                                        ]
                                    , else_ = Expression'LiteralValue
                                        ( Number "17" )
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
                        ( Expression'Not
                            ( Expression'LessThanOrEquals
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
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "a"
                                        }
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