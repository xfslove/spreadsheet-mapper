package me.excel.tools.processor;

import me.excel.tools.factory.ModelFactory;
import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelSheet;
import me.excel.tools.utils.FieldValueSetter;
import me.excel.tools.utils.ReflectionValueSetter;

import java.util.ArrayList;
import java.util.List;

/**
 * object 处理器
 *
 * Created by hanwen on 15-12-16.
 */
public abstract class AbstractDataProcessor implements DataProcessor {

  protected ModelFactory modelFactory;

  protected List<FieldValueSetter> customValueSetters = new ArrayList<>();

  protected ReflectionValueSetter reflectionValueSetter = new ReflectionValueSetter();

  @Override
  public void handle(ExcelSheet excelSheet) {

    if (excelSheet == null) {
      return;
    }

    List models = new ArrayList<>();

    excelSheet.getDataRows().forEach(row -> {

      Object model = modelFactory.create(row);

      List<ExcelCell> unsolved = new ArrayList<>();

      row.getCells().forEach(cell -> {

        boolean solved = false;
        for (FieldValueSetter customValueSetter : customValueSetters) {
          if (customValueSetter.matches(cell)) {
            customValueSetter.set(model, cell);
            solved = true;
            break;
          }
        }

        if (!solved) {
          unsolved.add(cell);
        }
      });
      reflectionValueSetter.set(model, unsolved);
      models.add(model);
    });

    handleModels(models);
  }

  protected abstract void handleModels(List models);

  @Override
  public void addFieldValueSetter(FieldValueSetter... setters) {
    if (setters == null) {
      return;
    }

    for (FieldValueSetter setter : setters) {
      this.customValueSetters.add(setter);
    }
  }

  @Override
  public void setModelFactory(ModelFactory modelFactory) {
    this.modelFactory = modelFactory;
  }

}
