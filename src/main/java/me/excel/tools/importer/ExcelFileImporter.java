package me.excel.tools.importer;

import me.excel.tools.factory.ModelFactory;
import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelSheet;
import me.excel.tools.model.excel.ExcelWorkbook;
import me.excel.tools.processor.DataProcessor;
import me.excel.tools.setter.DefaultValueSetter;
import me.excel.tools.setter.FieldValueSetter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * excel importer
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileImporter implements UserFileImporter {

  private ExcelWorkbook excelWorkbook;

  private ModelFactory modelFactory;

  private List<FieldValueSetter> fieldValueSetters = new ArrayList<>();

  private DefaultValueSetter defaultValueSetter = new DefaultValueSetter();

  public ExcelFileImporter(ExcelWorkbook excelWorkbook) {
    this.excelWorkbook = excelWorkbook;
  }

  @Override
  public void process(DataProcessor dataProcessor) {

    if (excelWorkbook == null) {
      throw new IllegalArgumentException("excel is null");
    }
    if (dataProcessor == null) {
      throw new IllegalArgumentException("dataProcessor is null");
    }

    ExcelSheet excelSheet = excelWorkbook.getFirstSheet();

    List models = new ArrayList<>();
    excelSheet.getDataRows().forEach(row -> {

      Object origin = modelFactory.create(row);
      Object model = modelFactory.create(row);

      dataProcessor.preProcessing(origin);

      defaultValueSetter.set(model, row.getCells());

      for (ExcelCell excelCell : row.getCells()) {

        for (FieldValueSetter customValueSetter : fieldValueSetters) {
          if (customValueSetter.matches(excelCell.getField())) {
            customValueSetter.set(model, excelCell);
            break;
          }
        }
      }

      dataProcessor.postProcessing(model);

      models.add(model);
    });

    dataProcessor.handle(models);
  }

  @Override
  public void addCellValueSetter(FieldValueSetter... setters) {
    if (setters == null) {
      return;
    }

    Collections.addAll(this.fieldValueSetters, setters);
  }

  @Override
  public void setModelFactory(ModelFactory modelFactory) {
    this.modelFactory = modelFactory;
  }
}