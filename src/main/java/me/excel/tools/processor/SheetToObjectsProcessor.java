package me.excel.tools.processor;

import me.excel.tools.exception.ExcelProcessException;
import me.excel.tools.factory.ModelFactory;
import me.excel.tools.model.excel.Cell;
import me.excel.tools.model.excel.Row;
import me.excel.tools.model.excel.Sheet;
import me.excel.tools.setter.DefaultValueSetter;
import me.excel.tools.setter.FieldValueSetter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * sheet to objects processor
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class SheetToObjectsProcessor<OBJECT> {

  private Sheet sheet;

  private ModelFactory<OBJECT> modelFactory;

  private Map<String, FieldValueSetter> key2fieldValueSetter = new HashMap<>();

  private DefaultValueSetter defaultValueSetter = new DefaultValueSetter();

  public SheetToObjectsProcessor(Sheet sheet) {
    this.sheet = sheet;
  }

  /**
   * @param objectProcessorListener processor
   * @see ObjectProcessorListener
   */
  public void process(ObjectProcessorListener<OBJECT> objectProcessorListener) {

    if (sheet == null) {
      throw new ExcelProcessException("sheet is null");
    }
    if (objectProcessorListener == null) {
      throw new ExcelProcessException("objectProcessorListener is null");
    }

    List<OBJECT> models = new ArrayList<>();

    for (Row row : sheet.getDataRows()) {

      OBJECT origin = modelFactory.create(row);
      objectProcessorListener.beforeRow(origin);

      OBJECT model = modelFactory.create(row);

      defaultValueSetter.set(model, row.getCells());

      for (Cell cell : row.getCells()) {

        FieldValueSetter fieldValueSetter = key2fieldValueSetter.get(cell.getField());

        if (fieldValueSetter != null) {
          fieldValueSetter.set(model, cell);
        }
      }

      objectProcessorListener.afterRow(model);

      models.add(model);

    }

    objectProcessorListener.afterSheet(models);
  }

  /**
   * @param setters field value setter
   * @see FieldValueSetter
   */
  public void addCellValueSetter(FieldValueSetter... setters) {
    if (setters == null) {
      return;
    }

    for (FieldValueSetter setter : setters) {
      key2fieldValueSetter.put(setter.getMatchField(), setter);
    }
  }

  /**
   * @param modelFactory model factory
   * @see ModelFactory
   */
  public void setModelFactory(ModelFactory<OBJECT> modelFactory) {
    this.modelFactory = modelFactory;
  }
}