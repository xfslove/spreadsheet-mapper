package me.excel.tools.processor;

import com.sun.tools.internal.ws.processor.ProcessorException;
import me.excel.tools.exception.ExcelProcessException;
import me.excel.tools.model.excel.Cell;
import me.excel.tools.model.excel.Row;
import me.excel.tools.model.excel.Sheet;
import me.excel.tools.model.excel.Workbook;
import me.excel.tools.setter.DefaultValueSetter;
import me.excel.tools.setter.FieldValueSetter;
import org.apache.commons.collections.MapUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * sheet to objects processor
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class DefaultObjectProcessor implements ObjectProcessor {

  private Workbook workbook;

  private Map<Integer, ObjectFactory> sheetIndex2modelFactory = new HashMap<>();

  private Map<Integer, ObjectProcessorListener> sheetIndex2listener = new HashMap<>();

  private Map<Integer, Map<String, FieldValueSetter>> key2fieldValueSetter = new HashMap<>();

  private DefaultValueSetter defaultValueSetter = new DefaultValueSetter();

  public DefaultObjectProcessor(Workbook workbook) {
    this.workbook = workbook;
  }

  @Override
  public void addCellValueSetter(FieldValueSetter... setters) {
    if (setters == null) {
      return;
    }

    for (FieldValueSetter setter : setters) {

      int sheetIndex = setter.getSheetIndex();

      if (!key2fieldValueSetter.containsKey(sheetIndex)) {
        key2fieldValueSetter.put(sheetIndex, new HashMap<>());
      }

      key2fieldValueSetter.get(sheetIndex).put(setter.getMatchField(), setter);
    }
  }


  @Override
  public void addObjectProcessorListener(ObjectProcessorListener... objectProcessorListeners) {
    if (objectProcessorListeners == null) {
      return;
    }
    for (ObjectProcessorListener listener : objectProcessorListeners) {
      sheetIndex2listener.put(listener.getSheetIndex(), listener);
    }
  }

  @Override
  public void addModelFactory(ObjectFactory... objectFactories) {
    if (objectFactories == null) {
      return;
    }
    for (ObjectFactory factory : objectFactories) {
      sheetIndex2modelFactory.put(factory.getSheetIndex(), factory);
    }
  }

  @Override
  public List<List<Object>> process() {

    if (MapUtils.isEmpty(sheetIndex2modelFactory)) {
      throw new ExcelProcessException("no model factory to create object");
    }

    if (workbook == null) {
      throw new ExcelProcessException("workbook is null");
    }

    List<List<Object>> objects = new ArrayList<>();

    for (Sheet sheet : workbook.getSheets()) {

      int sheetIndex = sheet.getIndex();

      ObjectFactory objectFactory = sheetIndex2modelFactory.get(sheetIndex);

      if (objectFactory == null) {
        throw new ProcessorException("no model factory to create object of sheet " + sheetIndex);
      }

      ObjectProcessorListener listener = sheetIndex2listener.get(sheetIndex);
      if (listener == null) {
        listener = new NoopObjectProcessorListener(sheetIndex);
      }

      List<Object> oneSheetObjects = new ArrayList<>();

      listener.beforeSheet(sheet, oneSheetObjects);

      for (Row row : sheet.getDataRows()) {

        Object origin = objectFactory.create(row);
        listener.beforeRow(row, origin);

        Object model = objectFactory.create(row);

        defaultValueSetter.set(model, row.getCells());

        for (Cell cell : row.getCells()) {

          Map<String, FieldValueSetter> valueSetterOfSheet = key2fieldValueSetter.get(sheetIndex);

          if (MapUtils.isEmpty(valueSetterOfSheet)) {
            continue;
          }

          FieldValueSetter fieldValueSetter = valueSetterOfSheet.get(cell.getField());

          if (fieldValueSetter != null) {
            fieldValueSetter.set(model, cell);
          }
        }

        listener.afterRow(row, model);

        oneSheetObjects.add(model);

      }

      listener.afterSheet(sheet, oneSheetObjects);

      objects.add(oneSheetObjects);
    }


    return objects;
  }
}