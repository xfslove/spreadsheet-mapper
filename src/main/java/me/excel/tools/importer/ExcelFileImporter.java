package me.excel.tools.importer;

import me.excel.tools.factory.FileTemplate;
import me.excel.tools.factory.ModelFactory;
import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelSheet;
import me.excel.tools.model.excel.ExcelWorkbook;
import me.excel.tools.processor.DataProcessor;
import me.excel.tools.setter.CellValueSetter;
import me.excel.tools.setter.DefaultValueSetter;

import java.util.ArrayList;
import java.util.List;

/**
 * excel 文件导入器
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileImporter implements UserFileImporter {

  protected FileTemplate fileTemplate;

  protected ExcelWorkbook excelWorkbook;

  protected ModelFactory modelFactory;

  protected List<CellValueSetter> cellValueSetters = new ArrayList<>();

  protected DefaultValueSetter defaultValueSetter = new DefaultValueSetter();

  public ExcelFileImporter(FileTemplate fileTemplate, ExcelWorkbook excelWorkbook) {
    this.fileTemplate = fileTemplate;
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

      dataProcessor.preProcessing(model);

      defaultValueSetter.set(model, row.getCells());

      for (ExcelCell excelCell : row.getCells()) {

        for (CellValueSetter customValueSetter : cellValueSetters) {
          if (customValueSetter.matches(excelCell)) {
            customValueSetter.set(model, excelCell);
            break;
          }
        }
      }

      dataProcessor.postProcessing(origin, model);

      models.add(model);
    });

    dataProcessor.handle(models);
  }

  @Override
  public void addCellValueSetter(CellValueSetter... setters) {
    if (setters == null) {
      return;
    }

    for (CellValueSetter setter : setters) {
      this.cellValueSetters.add(setter);
    }
  }

  @Override
  public void setModelFactory(ModelFactory modelFactory) {
    this.modelFactory = modelFactory;
  }
}