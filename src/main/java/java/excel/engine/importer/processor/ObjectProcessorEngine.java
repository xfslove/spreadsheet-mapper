package java.excel.engine.importer.processor;

import java.excel.engine.importer.setter.FieldValueSetter;
import java.excel.engine.model.ext.SheetContext;
import java.util.List;

/**
 * object processor
 * <p>
 * Created by hanwen on 2016/12/28.
 */
public interface ObjectProcessorEngine {

  /**
   * <pre>
   * field value setter unique with object field in one sheet (one to one),
   * if you add setter with same match field({@link FieldValueSetter#getMatchField()}),
   * after add will override before add
   * </pre>
   *
   * @param setters field value setter
   * @see FieldValueSetter
   */
  void addFieldValueSetter(FieldValueSetter... setters);

  /**
   * <pre>
   * one sheet one object template
   * if you add object factory with same sheet index({@link ObjectFactory#getSheetIndex()}),
   * after add will override before add
   * </pre>
   *
   * @param objectFactories object factories
   * @see ObjectFactory
   */
  void addObjectFactory(ObjectFactory... objectFactories);

  /**
   * <pre>
   * one sheet one listener
   * if you add listener with same sheet index({@link ObjectProcessorListener#getSheetIndex()}),
   * after add will override before add
   * </pre>
   *
   * @param objectProcessorListeners listeners
   * @see ObjectProcessorListener
   */
  void addObjectProcessorListener(ObjectProcessorListener... objectProcessorListeners);

  /**
   * @return list of sheets data
   * @see SheetContext
   */
  List<SheetContext> process();
}
