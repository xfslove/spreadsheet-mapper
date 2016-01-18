package me.excel.tools.processor;

import java.util.List;
import java.util.function.Consumer;

/**
 * Created by hanwen on 16-1-18.
 */
public class CommonDataProcessor<D> implements DataProcessor {

  protected Consumer<D> modelAcceptor;

  public CommonDataProcessor(Consumer<D> modelAcceptor) {
    this.modelAcceptor = modelAcceptor;
  }

  @Override
  public void handle(List models) {
    models.forEach(model -> modelAcceptor.accept((D) model));
  }
}
