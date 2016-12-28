package me.excel.tools.processor;

import me.excel.tools.setter.FieldValueSetter;

import java.util.List;

/**
 * Created by hanwen on 2016/12/28.
 */
public interface ObjectProcessor {

  /**
   * cell value setter unique with object field in one sheet (one to one), after add will override before add
   *
   * @param setters field value setter
   * @see FieldValueSetter
   */
  void addCellValueSetter(FieldValueSetter... setters);

  /**
   * one sheet, one model factory
   *
   * @param objectFactories model factorys
   * @see ObjectFactory
   */
  void addModelFactory(ObjectFactory... objectFactories);

  /**
   * one sheet one listener
   *
   * @param objectProcessorListeners listeners
   * @see ObjectProcessorListener
   */
  void addObjectProcessorListener(ObjectProcessorListener... objectProcessorListeners);

  /**
   * @return list of sheets data
   */
  List<List<Object>> process();
}
