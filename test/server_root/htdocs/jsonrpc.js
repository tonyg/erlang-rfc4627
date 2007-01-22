JsonRpcRequestId = 1;

JsonRpcTransaction = Class.create();
Object.extend(JsonRpcTransaction.prototype,
{
    initialize: function(serviceUrl, methodName, params, options) {
	this.options = {
	    asynchronous: true,
	    debug: false
	};
	Object.extend(this.options, options || {});
	this.serviceUrl = serviceUrl;
	this.methodName = methodName;
	this.params = params;
	this.reply = null;
	this.replyReady = 0;
	this.callbacks = [];
	this.request =
	    new Ajax.Request(serviceUrl,
			     { method: 'post',
			       requestHeaders: ['Content-type', 'application/json',
						'Accept', 'application/json'],
			       postBody: JSON.stringify({ version: "1.1",
							  id: JsonRpcRequestId++,
							  method: methodName,
							  params: params }),
			       asynchronous: this.options.asynchronous,
			       onComplete: this.receiveReply.bind(this) });
	if (!this.options.asynchronous) {
	    this.receiveReply(this.request.transport);
	}
    },

    receiveReply: function(ajaxRequest) {
	var response = JSON.parse(ajaxRequest.responseText);
	if (response.error) {
	    if (this.options.debug) {
		alert("JsonRPC error:\n" +
		      "Service: " + this.serviceUrl + "\n" +
		      "Method: " + this.methodName + "\n" +
		      "Params: " + JSON.stringify(this.params) + "\n" +
		      "Response: " + JSON.stringify(response) + "\n");
	    }
	    throw response.error;
	} else {
	    var reply = response.result;
	    this.reply = reply;
	    this.replyReady = 1;
	    this.callbacks.each(function (cb) { cb(reply); });
	}
    },

    addCallback: function(cb) {
	this.callbacks.push(cb);
	if (this.replyReady) {
	    cb(this.reply);
	}
	return this;
    }
});

JsonRpcService = Class.create();
Object.extend(JsonRpcService.prototype,
{
    initialize: function(serviceUrl, options) {
	this.options = {
	    debug: false
	};
	Object.extend(this.options, options || {});
	this.serviceUrl = serviceUrl;
	var txn = new JsonRpcTransaction(serviceUrl,
					 "system.describe",
					 [],
					 {asynchronous: false,
					  debug: this.options.debug});
	this.serviceDescription = txn.reply;
	var svc = this;
	this.serviceDescription.procs.each(function (desc) {
					       svc[desc.name] = svc.makeGenericProxy(desc);
					   });
    },

    makeGenericProxy: function(desc) {
	return function () {
	    var actuals = $A(arguments);
	    return new JsonRpcTransaction(this.serviceDescription.address,
					  desc.name,
					  actuals,
					  {debug: this.options.debug});
	};
    }
});
