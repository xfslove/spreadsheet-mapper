package me.excel.tools.processor;

import me.excel.tools.exception.ExcelProcessException;
import me.excel.tools.factory.ModelFactory;
import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.model.excel.ExcelSheet;
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

  private ExcelSheet excelSheet;

  private ModelFactory<OBJECT> modelFactory;

  private Map<String, FieldValueSetter> key2fieldValueSetter = new HashMap<>();

  private DefaultValueSetter defaultValueSetter = new DefaultValueSetter();

  public SheetToObjectsProcessor(ExcelSheet excelSheet) {
    this.excelSheet = excelSheet;
  }

  /**
   * @param dataProcessorListener processor
   * @see DataProcessorListener
   */
  public void process(DataProcessorListener dataProcessorListener) {

    if (excelSheet == null) {
      throw new ExcelProcessException("sheet is null");
    }
    if (dataProcessorListener == null) {
      throw new ExcelProcessException("dataProcessorListener is null");
    }

    List<OBJECT> models = new ArrayList<>();

    for (ExcelRow excelRow : excelSheet.getDataRows()) {

      OBJECT origin = modelFactory.create(excelRow);
      dataProcessorListener.beforeRow(origin);

      OBJECT model = modelFactory.create(excelRow);

      defaultValueSetter.set(model, excelRow.getCells());

      for (ExcelCell excelCell : excelRow.getCells()) {

        FieldValueSetter fieldValueSetter = key2fieldValueSetter.get(excelCell.getField());

        if (fieldValueSetter != null) {
          fieldValueSetter.set(model, excelCell);
        }
      }

      dataProcessorListener.afterRow(model);

      models.add(model);

    }

    dataProcessorListener.afterSheet(models);
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